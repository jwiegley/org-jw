{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Org.Lint where

import Control.Applicative
import Control.DeepSeq
import Control.Lens
import Control.Monad (foldM, unless, when)
import Control.Monad.Reader
import Control.Monad.Writer
import Crypto.Hash.SHA512
import Data.ByteString.Base16 qualified as Base16
import Data.Char (
  isAsciiLower,
  isAsciiUpper,
  isDigit,
  isLower,
  isSpace,
  isUpper,
  toLower,
 )
import Data.Data (Data)
import Data.Data.Lens
import Data.Foldable (Foldable (..), forM_)
import Data.List (dropWhileEnd, intercalate, isInfixOf, isPrefixOf, isSuffixOf)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Typeable (Typeable)
import Debug.Trace (traceM)
import GHC.Generics (Generic)
import Org.Data
import Org.Print
import Org.Types
import System.Directory (
  doesDirectoryExist,
  doesFileExist,
  doesPathExist,
  getHomeDirectory,
 )
import System.Exit
import System.FilePath.Posix
import System.IO.Unsafe (unsafePerformIO)
import System.Process
import Text.Regex.TDFA
import Text.Regex.TDFA.String ()
import Text.Show.Pretty
import Prelude hiding (Foldable (..))

consistent :: (Eq a) => [a] -> Bool
consistent [] = True
consistent (x : xs) = foldl' (\b y -> b && x == y) True xs

data LintMessageKind = LintDebug | LintAll | LintInfo | LintWarn | LintError
  deriving (Data, Show, Eq, Typeable, Generic, Enum, Bounded, Ord, NFData)

parseLintMessageKind :: String -> Maybe LintMessageKind
parseLintMessageKind = \case
  "ERROR" -> Just LintError
  "WARN" -> Just LintWarn
  "INFO" -> Just LintInfo
  "ALL" -> Just LintAll
  "DEBUG" -> Just LintDebug
  _ -> Nothing

{- | Which blog a published post belongs to. The poetry-vs-code lint rules
(BlogSrcBlockOnPoetrySite, BlogVerseBlockOnCodeSite) need to know this, and
it is determined entirely by the file's @#+filetags:@ (see 'blogSite').
-}
data Site
  = -- | johnwiegley: personal essays and poetry; uses @#+begin_verse@.
    PoetrySite
  | -- | newartisans: technical posts and code; uses @#+begin_src@.
    CodeSite
  deriving (Data, Show, Eq, Typeable, Generic, Enum, Bounded, Ord, NFData)

{- | Extra context for the stricter blog-post lint rules. Threaded through
'lintOrgFiles', 'lintOrgFile', 'lintOrgFile'' and 'lintOrgEntry' alongside
the 'LintMessageKind' level.

The blog rules are no longer gated on a flag: they activate automatically for
any file whose @#+filetags:@ contain @posts@ (see 'isBlogPost'). This record
now only carries the cross-file post-ID universe used to resolve @id:@ links.
-}
newtype LintMode = LintMode
  { _lintPostIds :: Set String
  {- ^ The universe of all post @:ID:@s in the corpus, lower-cased, used to
  resolve @[[id:UUID]]@ links (see 'BlogUnresolvedIdLink'). Built
  unconditionally on every run in 'lintOrgFiles' across every file, so the
  always-on 'BlogUnresolvedIdLink' check never sees an empty set.
  -}
  }
  deriving (Show, Eq, Generic, NFData)

{- | The default mode: no known post IDs. 'lintOrgFiles' overrides
@_lintPostIds@ with the corpus-wide set before linting each file; direct
'lintOrgFile' callers that do not resolve @id:@ links can use this as-is.
-}
defaultLintMode :: LintMode
defaultLintMode =
  LintMode
    { _lintPostIds = Set.empty
    }

data TransitionKind
  = FirstTransition
  | IntermediateTransition
  | LastTransition
  deriving (Show, Eq, Generic, NFData)

data LintMessageCode
  = TodoMissingProperty String
  | FileMissingProperty String
  | TaskMissingAssignment
  | TodoLinkDoesNotMatchUrl
  | TodoFileDoesNotMatchAttachment
  | ArchiveTagFileDoesNotExist FilePath
  | TodoLinkKeywordImpliesLinkTag
  | FileSlugMismatch String
  | MisplacedProperty
  | MisplacedTimestamp
  | MisplacedLogEntry
  | MisplacedDrawerEnd
  | DuplicateFileProperty String
  | DuplicateProperty String
  | DuplicateTag String
  | DuplicatedIdentifier String
  | InvalidStateChangeTransitionNotAllowed String (Maybe String) [String]
  | InvalidStateChangeInvalidTransition TransitionKind String String
  | InvalidStateChangeWrongTimeOrder Time Time
  | InvalidStateChangeIdempotent String
  | MultipleLogbooks
  | MixedLogbooks
  | WhitespaceAtStartOfLogEntry
  | FileTitleMissing
  | TitleWithExcessiveWhitespace
  | OverlyLongHeadline
  | TimestampsOnNonTodo
  | InconsistentWhitespace String
  | InconsistentFilePreambleWhitespace
  | UnnecessaryWhitespace
  | EmptyBodyWhitespace
  | MultipleBlankLines
  | CategoryTooLong String
  | FileCreatedTimeMismatch Time Time
  | TitlePropertyNotLast
  | FileTagsTodoMismatch
  | VerbInFileUnknown String
  | TagInFileUnknown String
  | InvalidLocation String
  | InvalidDrawerCase DrawerType
  | TodoMissingReviewProperties
  | NonTodoWithReviewProperties
  | BrokenLink String
  | HashesDoNotMatch String String
  | FileFailsToRoundTrip
  | AudioFileNotFound FilePath
  | -- Blog-strict rules (auto-enabled on files tagged :posts:)
    BlogLegacyFileLink String
  | BlogUnresolvedIdLink String
  | BlogAbsoluteInternalLink String
  | BlogNonWebLinkScheme String
  | BlogMissingRelativeImage FilePath
  | BlogRawHtmlExportBlock
  | BlogNonHtmlExportBlock String
  | BlogSrcBlockOnPoetrySite
  | BlogVerseBlockOnCodeSite
  | {- | Inline emphasis whose body spans 3+ source lines (>= 2 newlines):
    Org/Pandoc leaks the markers as literal text. Carries the marker char
    and a one-line snippet of the offending span.
    -}
    BlogMultilineEmphasis Char String
  deriving (Show, Eq, Generic, NFData)

data LintMessage = LintMessage
  { lintMsgPos :: Int
  , lintMsgKind :: LintMessageKind
  , lintMsgCode :: LintMessageCode
  }
  deriving (Show, Eq, Generic, NFData)

-- Helpers shared by the blog-strict rules. -----------------------------------

{- | The colon-delimited tags of a file's @#+filetags:@ line, lower-cased.
These live in @_headerFileProperties@ as a @Property@ named @"filetags"@
whose value is the raw @:a:b:c:@ string; 'tagList' splits it.
-}
blogFiletags :: OrgFile -> [String]
blogFiletags org =
  map (map toLower . (^. tagString)) $
    org ^. orgFileProperty "filetags" . from tagList

{- | True if a file is a published blog post, i.e. its filetags contain
@posts@. All blog-strict rules are gated on this (in addition to @--blog@).
-}
isBlogPost :: OrgFile -> Bool
isBlogPost org = "posts" `elem` blogFiletags org

{- | Which blog a post belongs to, by filetag: @johnwiegley@ → 'PoetrySite',
@newartisans@ → 'CodeSite'. 'Nothing' if neither (or somehow both) is set,
in which case the poetry-vs-code rules stay silent.
-}
blogSite :: OrgFile -> Maybe Site
blogSite org =
  case (hasTag "johnwiegley", hasTag "newartisans") of
    (True, False) -> Just PoetrySite
    (False, True) -> Just CodeSite
    _ -> Nothing
 where
  tags = blogFiletags org
  hasTag t = t `elem` tags

{- | Every bracket-link target appearing in a chunk of body text. A target is
the text between @[[@ and the first @]@ (which terminates the target in both
@[[target]]@ and @[[target][desc]]@ forms). Inline markup and links are not
modeled by the parser, so this is a raw regex scan, matching the shallow
parser caveat in the construct catalog. Bare (non-bracketed) @http://@ URLs
are intentionally ignored — only explicit @[[...]]@ links are linted.
-}
bracketLinkTargets :: String -> [String]
bracketLinkTargets paragraph =
  map (drop 2) $
    getAllTextMatches (paragraph =~ ("\\[\\[[^]]+" :: String))

{- | The lower-cased scheme of a link target, if it has one. A scheme is a
leading run of @[A-Za-z][A-Za-z0-9+.-]*@ immediately followed by a colon
(matching the RFC-3986 shape). @id:@, @file:@, @https:@, @ftp:@, etc. all
qualify; @./foo@, @/bar@ and @#anchor@ do not.
-}
linkScheme :: String -> Maybe String
linkScheme target =
  case break (== ':') target of
    (c : cs, ':' : _)
      | isSchemeStart c
      , all isSchemeChar cs ->
          Just (map toLower (c : cs))
    _ -> Nothing
 where
  isSchemeStart c = isAsciiLower c || isAsciiUpper c
  isSchemeChar c =
    isSchemeStart c
      || isDigit c
      || c `elem` ['+', '.', '-']

{- | The lower-cased begin-block keyword of a drawer, e.g. a @#+begin_src foo@
block (stored by the parser as @BeginDrawer "#+begin_src foo"@) yields
@"#+begin_src"@. Plain (non-@#+begin@) drawers yield 'Nothing'.
-}
drawerBeginKind :: DrawerType -> Maybe String
drawerBeginKind (PlainDrawer _) = Nothing
drawerBeginKind (BeginDrawer label) =
  case words (map toLower label) of
    (w : _) | "#+begin" `isPrefixOf` w -> Just w
    _ -> Nothing

{- | The export backend of a @#+begin_export <backend>@ drawer, lower-cased
(e.g. @html@, @latex@). 'Nothing' for any other block. Reads the begin line
out of the drawer's raw content (its first element) so it works regardless
of how the @BeginDrawer@ label was truncated.
-}
exportBackend :: Block -> Maybe String
exportBackend (Drawer _ ty (beginLine : _))
  | Just kind <- drawerBeginKind ty
  , kind == "#+begin_export" =
      case drop 1 (words (map toLower beginLine)) of
        (backend : _) -> Just backend
        [] -> Just ""
exportBackend _ = Nothing

{- | The normalized body lines of an export block: the raw content with the
@#+begin_export@ and @#+end_export@ delimiter lines stripped, each remaining
line trimmed of leading/trailing whitespace, and blank lines dropped entirely.

Whitespace normalization matters because johnwiegley indents its teaser two
spaces (@\"  <!--more-->\"@) while newartisans keeps it flush-left; both must
normalize to @[\"<!--more-->\"]@ so 'isMoreTeaserBlock' exempts them equally.
-}
exportBlockBody :: Block -> [String]
exportBlockBody (Drawer _ _ ls) =
  filter (not . null) (map trim (dropDelims ls))
 where
  trim = dropWhileEnd isSpace . dropWhile isSpace
  dropDelims =
    filter
      ( \l ->
          let l' = map toLower (trim l)
           in not
                ( "#+begin_export" `isPrefixOf` l'
                    || "#+end_export" `isPrefixOf` l'
                )
      )
exportBlockBody _ = []

{- | Is this export block just the benign @<!--more-->@ WordPress teaser
marker (ignoring indentation and blank lines)? Such a block renders on the
website and correctly disappears from the PDF, so it is exempt from
'BlogRawHtmlExportBlock'.
-}
isMoreTeaserBlock :: Block -> Bool
isMoreTeaserBlock blk = exportBlockBody blk == ["<!--more-->"]

{- | The source 'Loc' of any 'Block'. Every 'Block' constructor carries its
'Loc' as the first field; blog findings report at this position so that
preamble-body findings land on the construct's own line rather than at the
end of the preamble.
-}
blockLoc :: Block -> Loc
blockLoc (Whitespace loc _) = loc
blockLoc (Paragraph loc _) = loc
blockLoc (Drawer loc _ _) = loc
blockLoc (InlineTask loc _) = loc

{- | Does a link target name an image (by extension)? Used to decide whether a
relative @[[./...]]@ / @[[file:images/...]]@ link should be checked for
on-disk existence as an image.
-}
isImageTarget :: String -> Bool
isImageTarget target =
  any
    (`isSuffixOfCI` stripDescAndAnchor target)
    [".png", ".jpg", ".jpeg", ".gif", ".svg", ".webp", ".tiff", ".tif", ".bmp"]
 where
  stripDescAndAnchor = takeWhile (`notElem` ['#'])
  isSuffixOfCI suf s = map toLower suf `isSuffixOf` map toLower s

{- | The inline-emphasis markers Org/Pandoc recognize: @/italic/@, @*bold*@,
@_underline_@, @=verbatim=@, @~code~@.
-}
emphasisMarkers :: [Char]
emphasisMarkers = "/*_=~"

{- | May this character precede an OPENING emphasis marker? Mirrors the
reference detector's @OPEN_PREV@ set, generalized so that any whitespace
(including a newline, i.e. start-of-line) qualifies, per the rule "at
start-of-line OR preceded by whitespace or one of @-(['\"{@".
-}
isEmphOpenPrev :: Char -> Bool
isEmphOpenPrev c = isSpace c || c `elem` ['-', '(', '[', '\'', '"', '{']

{- | May this character follow a CLOSING emphasis marker? Mirrors the reference
detector's @CLOSE_NEXT@ set: end-of-line, whitespace, or one of the closing
punctuation characters.
-}
isEmphCloseNext :: Char -> Bool
isEmphCloseNext c =
  isSpace c
    || c `elem` ['-', '.', ',', ';', ':', '!', '?', '\'', ')', '}', '[', '"']

{- | Blank out the interior of every @[[...]]@ link in a line, replacing each
non-bracket character of the link with a space (preserving length so byte
offsets stay valid). This keeps URL/path slashes inside links from
false-positiving as emphasis markers.
-}
blankBracketLinks :: String -> String
blankBracketLinks = go False
 where
  go _ [] = []
  go _ ('[' : '[' : rest) = '[' : '[' : go True rest
  go _ (']' : ']' : rest) = ']' : ']' : go False rest
  go True (c : rest)
    | c == '\n' = '\n' : go True rest
    | otherwise = ' ' : go True rest
  go False (c : rest) = c : go False rest

{- | Find inline-emphasis spans whose body crosses 2 or more newlines (3+
source lines), which Org/Pandoc renders by leaking the markers as literal
text. Operates on one contiguous prose run (lines joined by @\\n@), with link
interiors already blanked. Returns @(charOffsetOfOpenMarker, markerChar,
snippet)@ for each offending span, where the snippet is the span body with
newlines flattened to spaces and truncated for the message.

Mirrors the reference @find_breaking@ semantics: an opening marker must sit at
a valid open position (see 'isEmphOpenPrev') and be immediately followed by a
non-space that is not the same marker; the matching close must be immediately
preceded by a non-space and followed by a valid close character (see
'isEmphCloseNext'); the body must be non-empty; only spans with >= 2 newlines
are reported. The search for a close abandons a candidate open once it has
scanned more than three intervening newlines.
-}
findMultilineEmphasis :: String -> [(Int, Char, String)]
findMultilineEmphasis runBody = go 0
 where
  arr = runBody
  n = length arr
  charAt k = arr !! k

  go i
    | i >= n = []
    | c `elem` emphasisMarkers
    , isEmphOpenPrev prev
    , not (isSpace nxt)
    , nxt /= c =
        case findClose c (i + 1) of
          Just j ->
            let spanStr = take (j - i + 1) (drop i arr)
                bodyInner = take (j - i - 1) (drop (i + 1) arr)
             in if not (null bodyInner) && countNewlines spanStr >= 2
                  then (i, c, snippetOf spanStr) : go (j + 1)
                  else go (j + 1)
          Nothing -> go (i + 1)
    | otherwise = go (i + 1)
   where
    c = charAt i
    prev = if i > 0 then charAt (i - 1) else '\n'
    nxt = if i + 1 < n then charAt (i + 1) else '\n'

  -- Locate the closing marker for an open of char @c@ starting at index
  -- @from0@.
  findClose c from0 = loop from0
   where
    loop j
      | j >= n = Nothing
      | charAt j == c
      , let p = charAt (j - 1)
      , let q = if j + 1 < n then charAt (j + 1) else '\n'
      , not (isSpace p)
      , isEmphCloseNext q =
          Just j
      | charAt j == '\n'
      , countNewlines (take (j - from0 + 1) (drop from0 arr)) > 3 =
          Nothing
      | otherwise = loop (j + 1)

  countNewlines = length . filter (== '\n')

  -- Flatten newlines to spaces and cap the length for the lint message.
  snippetOf s =
    let flat = map (\ch -> if ch == '\n' then ' ' else ch) s
     in if length flat > 90 then take 90 flat ++ "..." else flat

{- | Run every body-scanning blog-strict rule over one 'Body' (a file preamble
or an entry body), reporting through the supplied callback. The callback is
given the 'Loc' of the 'Block' containing each finding so that messages land
on the construct's own line (critical for the 975/1018 headline-less
johnwiegley posts, whose content lives entirely in the file preamble), rather
than at a single fixed position. The callback still applies the severity
threshold. Shared by 'lintOrgFile'' and 'lintOrgEntry'.

All checks are raw regex/string scans over the shallow parser's output, per
the construct catalog: links come from paragraph text, blocks from the
@#+begin_X@ drawers.
-}
blogBodyChecks ::
  Config ->
  LintMode ->
  OrgFile ->
  (Loc -> LintMessageKind -> LintMessageCode -> Writer [LintMessage] ()) ->
  Body ->
  Writer [LintMessage] ()
blogBodyChecks cfg mode org report bodyToScan = do
  -- Link rules: classify every [[...]] target, reporting at the Loc of the
  -- paragraph block it appears in.
  forM_ (bodyToScan ^.. blocks . traverse . filtered (has _Paragraph)) $ \blk ->
    forM_ (concatMap bracketLinkTargets (runReader (showBlock "" blk) cfg)) $ \target -> do
      let report' = report (blockLoc blk)
          scheme = linkScheme target
      -- RULE: legacy [[file:...]] links (broken in both web and PDF). Fires
      -- regardless of IGNORE_LINKS: the whole point is to surface these.
      when (scheme == Just "file") $
        report' LintError (BlogLegacyFileLink target)
      -- RULE: [[id:UUID]] whose UUID is not a known post/entry :ID:.
      forM_ (idLinkUuid target) $ \uuid ->
        unless (map toLower uuid `Set.member` _lintPostIds mode) $
          report' LintError (BlogUnresolvedIdLink uuid)
      -- RULE: [[/abs/path]] site-absolute permalink.
      when ("/" `isPrefixOf` target) $
        report' LintWarn (BlogAbsoluteInternalLink target)
      -- RULE: bracket link whose scheme is not http/https/id/mailto. @file@ is
      -- left to the dedicated BlogLegacyFileLink Error above so it is not
      -- double-flagged. @mailto@ works in both the website and PDF.
      forM_ scheme $ \s ->
        unless (s `elem` ["http", "https", "id", "mailto", "file"]) $
          report' LintWarn (BlogNonWebLinkScheme target)
      -- RULE: relative image whose file is missing on disk.
      when (isRelativeImage target && isImageTarget target) $ do
        let path = relativeImagePath target
        unless (pathExists cfg doesFileExist (org ^. orgFilePath) path) $
          report' LintError (BlogMissingRelativeImage path)

  -- Block rules: inspect each #+begin_X drawer, reporting at its Loc.
  forM_ (bodyToScan ^.. blocks . traverse . filtered (has _Drawer)) $ \blk -> do
    let report' = report (blockLoc blk)
        mkind = blk ^? _Drawer . _2 . to drawerBeginKind . _Just
    -- RULE: #+begin_export blocks. html with anything but the <!--more-->
    -- teaser is dropped from the PDF; non-html backends are dropped on the
    -- web and risk breaking xelatex.
    forM_ (exportBackend blk) $ \backend ->
      if backend == "html"
        then
          unless (isMoreTeaserBlock blk) $
            report' LintWarn BlogRawHtmlExportBlock
        else report' LintWarn (BlogNonHtmlExportBlock backend)
    -- RULE (poetry-vs-code): #+begin_src on the poetry site, #+begin_verse
    -- on the code site. Each keys off the file's site (its filetags).
    case blogSite org of
      Just PoetrySite ->
        when (mkind == Just "#+begin_src") $
          report' LintError BlogSrcBlockOnPoetrySite
      Just CodeSite ->
        when (mkind == Just "#+begin_verse") $
          report' LintError BlogVerseBlockOnCodeSite
      Nothing -> pure ()

  -- RULE: inline emphasis whose body spans 3+ source lines (>= 2 newlines).
  -- Org/Pandoc leaks the markers as literal text in that case. We scan
  -- contiguous prose runs (stitched across blank lines, broken at blocks and
  -- at headline/#+keyword/:drawer lines), with link interiors blanked out,
  -- and report at the line where the span opens.
  forM_ proseRuns $ \run -> do
    let bodyText = blankBracketLinks (intercalate "\n" (map snd run))
    forM_ (findMultilineEmphasis bodyText) $ \(off, marker, snippet) ->
      report (Loc (org ^. orgFilePath) (lineposAt run off)) LintWarn $
        BlogMultilineEmphasis marker snippet
 where
  -- The UUID of an [[id:UUID]] link (with any [desc] already stripped, since
  -- the target ends at the first ']').
  idLinkUuid target = case break (== ':') target of
    ("id", ':' : rest) -> Just rest
    _ -> Nothing

  -- Contiguous prose runs from the body, each a list of (bytePos, lineText)
  -- entries (one per source line). Runs break at Drawer/InlineTask blocks and
  -- at lines that begin a headline/#+keyword/:drawer (mirroring the reference
  -- detector's "break span continuity" cases), so an emphasis span is never
  -- considered to cross such a boundary.
  proseRuns :: [[(Int, String)]]
  proseRuns = splitRuns (concatMap blockLines (bodyToScan ^. blocks))

  -- Each (Int, String): Nothing-like break markers are encoded as Left;
  -- prose lines as Right (pos, text). Drawer/InlineTask blocks emit a break.
  blockLines :: Block -> [Either () (Int, String)]
  blockLines (Paragraph loc ls) =
    -- The block's pos is the FIRST line; later lines step back by the length
    -- of the preceding lines plus their newlines (pos counts bytes from end).
    zipWith
      ( \i l ->
          if isRunBreakLine l
            then Left ()
            else Right (loc ^. pos - lineOffset i ls, l)
      )
      [0 ..]
      ls
  blockLines (Whitespace loc txt) = [Right (loc ^. pos, txt)]
  blockLines (Drawer _ _ _) = [Left ()]
  blockLines (InlineTask _ _) = [Left ()]

  -- Byte length consumed by the first @i@ lines of a block (each line plus
  -- the newline that follows it in the source).
  lineOffset i ls = sum [length l + 1 | l <- take i ls]

  -- A prose line that should break run continuity (headline / #+keyword /
  -- :drawer:), matching the reference detector's skip set.
  isRunBreakLine l =
    let s = dropWhile isSpace l
     in "*" `isPrefixOf` s || "#+" `isPrefixOf` s || ":" `isPrefixOf` s

  splitRuns :: [Either () (Int, String)] -> [[(Int, String)]]
  splitRuns xs = case break isBreak (dropWhile isBreak xs) of
    ([], []) -> []
    (run, rest) -> map fromRight run : splitRuns rest
   where
    isBreak (Left _) = True
    isBreak _ = False
    fromRight (Right r) = r
    fromRight (Left _) = error "splitRuns: unexpected break"

  -- Map a character offset within a run's joined body back to the byte
  -- position of the source line that contains it.
  lineposAt :: [(Int, String)] -> Int -> Int
  lineposAt run off = walk run off
   where
    walk [] _ = run ^?! _head . _1 -- offset past end; fall back to run start
    walk ((p, l) : rest) k
      | k <= length l = p
      | otherwise = walk rest (k - length l - 1) -- skip the line and its \n

  -- A relative-path image link: [[./...]] or [[file:images/...]] /
  -- [[file:./...]] (i.e. a file: link with a non-absolute, local target).
  isRelativeImage target =
    "./" `isPrefixOf` target
      || case break (== ':') target of
        ("file", ':' : rest) ->
          not ("/" `isPrefixOf` rest)
            && isNothing (linkScheme rest)
        _ -> False

  -- The on-disk path of a relative image link, relative to the post, with the
  -- file: scheme prefix and any [desc]/#anchor removed.
  relativeImagePath =
    takeWhile (/= '#') . stripFilePrefix
   where
    stripFilePrefix t = case break (== ':') t of
      ("file", ':' : rest) -> rest
      _ -> t

lintOrgFiles ::
  Config ->
  LintMode ->
  LintMessageKind ->
  [OrgFile] ->
  Map FilePath [LintMessage]
lintOrgFiles cfg mode level xs =
  let (entriesById, ms) = foldr doLint (M.empty, []) xs
      idMsgs = flip concatMap (M.assocs entriesById) $ \(k, loc :| locs) ->
        [ ( loc ^. file
          ,
            [ LintMessage
                (loc ^. pos)
                LintError
                (DuplicatedIdentifier k)
            ]
          )
        | not (null locs)
        ]
   in M.unionWith (<>) (M.fromList ms) (M.fromList idMsgs)
 where
  -- The universe of IDs that an [[id:...]] link may resolve to: every
  -- file-level :ID: (where headline-less blog posts keep theirs) plus every
  -- entry :ID:, all lower-cased for case-insensitive matching. Built once
  -- across all files and injected into the mode handed to each file. Mirrors
  -- the cross-file duplicate-:ID: collection just below.
  mode' = mode{_lintPostIds = allIds}

  allIds =
    Set.fromList $
      map (map toLower) $
        concatMap
          ( \org ->
              org ^.. orgFileProperty "ID"
                ++ org ^.. allEntries . entryId
          )
          xs

  doLint ::
    OrgFile ->
    (Map String (NonEmpty Loc), [(FilePath, [LintMessage])]) ->
    (Map String (NonEmpty Loc), [(FilePath, [LintMessage])])
  doLint org (entriesById, ms) =
    (entriesById', (org ^. orgFilePath, msgs) : ms)
   where
    entriesById' =
      (\f -> foldr f entriesById (org ^.. allEntries)) $ \e m ->
        let loc = e ^. entryLoc
         in maybe
              m
              ( \ident ->
                  m
                    & at ident
                      %~ Just
                        . maybe
                          (NE.singleton loc)
                          (NE.cons loc)
              )
              (e ^? entryId)
    msgs = lintOrgFile cfg mode' level org

lintOrgFile :: Config -> LintMode -> LintMessageKind -> OrgFile -> [LintMessage]
lintOrgFile cfg mode level org = execWriter (lintOrgFile' cfg mode level org)

lintOrgFile' ::
  Config -> LintMode -> LintMessageKind -> OrgFile -> Writer [LintMessage] ()
lintOrgFile' cfg mode level org = do
  when (level == LintDebug) $ do
    traceM $ "Linting " ++ (org ^. orgFilePath)
  -- RULE: All files must have titles
  ruleFileShouldHaveTitle
  -- RULE: All files must have ID and CREATED properties
  ruleFileShouldHaveIdAndCreated
  -- RULE: File slugs should reflect the file's title
  ruleSlugMustMatchTitle
  -- RULE: Filenames with dates should have matching CREATED
  ruleCreationTimeMatchesCreated
  -- RULE: Title file property is always last. This is needed for the sake of
  --       xeft and how it displays entry text.
  ruleTitleProperyAlwaysLast
  -- rule: :ARCHIVE: or #+archive: property alwayos refers to existing file
  ruleArchiveTagFileExists
  -- RULE: A filetags of :todo: should indicate open TODO entries
  ruleFileTagsTodo
  -- RULE: Files do not have NEXT/LAST_REVIEW properties
  ruleOnlyTodosReview
  -- RULE: All tags are part of the tags vocabulary, if specified
  ruleTagsVocabulary
  -- RULE: All verbs are part of the verb vocabulary, if specified
  ruleVerbVocabulary
  -- RULE: Check that all file links point to actual files
  ruleCheckAllLinks
  -- RULE: Check that AUDIO property points to an existing file
  ruleAudioFileExists
  -- BLOG RULES: stricter checks for published posts. These activate
  -- automatically for any file whose #+filetags: contain :posts: (no flag
  -- needed). Scan the file preamble here; lintOrgEntry scans each entry body
  -- (most johnwiegley posts are headline-less, so the preamble is where the
  -- content lives). Each finding reports at the byte position of the block it
  -- was found in.
  when (isBlogPost org) $
    blogBodyChecks
      cfg
      mode
      org
      (\loc -> report' (loc ^. pos))
      (org ^. orgFileHeader . headerPreamble)
  -- RULE: No duplicated file properties outside of link and tags
  forM_ (findDuplicates (props ^.. traverse . name . to (map toLower))) $ \nm ->
    unless (nm `elem` ["link", "tags"]) $
      report LintError (DuplicateFileProperty nm)
  -- RULE: Whitespace before and after body should match
  -- checkFor LintInfo (InconsistentFilePreambleWhitespace org) $
  --   org ^? fileHeader . headerPreamble . leadSpace
  --     /= org ^? fileHeader . headerPreamble . endSpace
  case reverse (org ^.. allEntries) of
    [] -> pure ()
    e : es -> do
      mapM_
        (lintOrgEntry cfg mode org False ignoreWhitespace level)
        (reverse es)
      lintOrgEntry cfg mode org True ignoreWhitespace level e
 where
  ignoreWhitespace = org ^? orgFileProperty "WHITESPACE" == Just "ignore"

  ruleFileShouldHaveTitle =
    when (isNothing (org ^? orgFileProperty "title")) $
      report LintInfo FileTitleMissing

  ruleFileShouldHaveIdAndCreated = do
    when (isNothing (org ^? orgFileProperty "ID")) $
      report LintInfo (FileMissingProperty "ID")
    when (isNothing (org ^? orgFileProperty "CREATED")) $
      report
        ( if isNothing (OrgItem org ^? fileCreatedTime)
            then LintWarn
            else LintInfo
        )
        (FileMissingProperty "CREATED")

  ruleSlugMustMatchTitle =
    unless (isJust (org ^? orgFileProperty "NOSLUG")) $
      forM_ (OrgItem org ^? fileSlug) $ \slug ->
        unless (OrgItem org ^? fileActualSlug == OrgItem org ^? fileSlug) $
          report LintInfo (FileSlugMismatch slug)

  ruleCreationTimeMatchesCreated =
    forM_
      ( (,)
          <$> OrgItem org ^? fileTimestamp
          <*> OrgItem org ^? fileCreatedTime
      )
      $ \(created, created') ->
        unless (created == created') $
          report LintWarn (FileCreatedTimeMismatch created created')

  ruleTitleProperyAlwaysLast =
    forM_
      ( org
          ^? orgFileHeader
            . headerFileProperties
            . _last
            . name
            . to (map toLower)
      )
      $ \lastProp ->
        unless (lastProp == "title") $
          report LintWarn TitlePropertyNotLast

  ruleArchiveTagFileExists =
    forM_ (org ^? orgFileProperty "ARCHIVE") $ \path -> do
      let path' = takeWhile (/= ':') path
      unless
        ( pathExists
            cfg
            doesFileExist
            (org ^. orgFilePath)
            path'
        )
        $ report LintWarn (ArchiveTagFileDoesNotExist path')

  ruleFileTagsTodo =
    unless (isJust (org ^? orgFileProperty "HAS_TODO")) $
      forM_ (OrgItem org ^? fileTags) $ \tags ->
        unless
          ( ( if PlainTag "todo" `elem` tags
                then id
                else not
            )
              $ or (org ^.. allEntries . keyword . to (isOpenTodo cfg))
          )
          $ report LintWarn FileTagsTodoMismatch

  ruleOnlyTodosReview =
    when
      ( isJust (org ^? orgFileProperty "LAST_REVIEW")
          || isJust (org ^? orgFileProperty "NEXT_REVIEW")
          || isJust (org ^? orgFileProperty "REVIEWS")
          || isJust (org ^? orgFileProperty "Effort")
      )
      $ report LintWarn NonTodoWithReviewProperties

  ruleTagsVocabulary =
    forM_ (org ^? orgFileProperty "TAGS_ALL") $ \tags -> do
      let tags' = words tags
      forM_ (org ^.. allEntries) $ \e ->
        forM_ (e ^.. entryTags . traverse) $ \(PlainTag entryTag) ->
          unless (entryTag `elem` tags') $
            report' (e ^. entryLoc . pos) LintWarn $
              TagInFileUnknown entryTag

  ruleVerbVocabulary =
    forM_ (org ^? orgFileProperty "VERB_ALL") $ \verbs -> do
      let verbs' = words verbs
      forM_ (org ^.. allEntries) $ \e ->
        forM_ (e ^.. entryVerb . _Just) $ \verb ->
          unless (verb `elem` verbs') $
            report' (e ^. entryLoc . pos) LintWarn $
              VerbInFileUnknown verb

  ruleCheckAllLinks =
    unless (isJust (org ^? orgFileProperty "IGNORE_LINKS")) $
      forM_ paragraphs $ \paragraph ->
        case paragraph
          =~ ("\\[\\[(file:|https?:)([^]:]+)" :: String) of
          AllTextSubmatches ([_, protocol, link] :: [String]) ->
            unless
              ( if protocol == "file"
                  then
                    pathExists
                      cfg
                      doesPathExist
                      (org ^. orgFilePath)
                      link
                  else level > LintAll || urlExists (protocol ++ link)
              )
              $ report
                LintError
                ( BrokenLink
                    ( if protocol == "file"
                        then link
                        else protocol ++ link
                    )
                )
          _ -> pure ()

  ruleAudioFileExists =
    forM_ (org ^? orgFileProperty "AUDIO") $ \audioPath ->
      unless
        ( pathExists
            cfg
            doesFileExist
            (org ^. orgFilePath)
            audioPath
        )
        $ report LintError (AudioFileNotFound audioPath)

  paragraphs = bodyString (has _Paragraph)

  bodyString f =
    org
      ^. orgFileHeader
        . headerPreamble
        . blocks
        . traverse
        . filtered f
        . to (\b -> runReader (showBlock "" b) cfg)

  props =
    org ^. orgFileHeader . headerPropertiesDrawer
      ++ org ^. orgFileHeader . headerFileProperties

  report' loc kind code
    | kind >= level = do
        when (level == LintDebug) $
          traceM $
            "file: " ++ ppShow org
        tell [LintMessage loc kind code]
    | otherwise = pure ()

  report = report' 1

lintOrgEntry ::
  Config ->
  LintMode ->
  OrgFile ->
  Bool ->
  Bool ->
  LintMessageKind ->
  Entry ->
  Writer [LintMessage] ()
lintOrgEntry cfg mode org isLastEntry ignoreWhitespace level e = do
  -- jww (2024-05-28): NYI
  -- RULE: No open keywords in archives
  -- RULE: No CREATED date lies in the future
  -- RULE: No title has special characters without escaping
  -- RULE: Leading and trailing whitespace is consistent within log entries
  -- RULE: There is no whitespace preceding the event log
  -- RULE: There is no whitespace after the PROPERTY block (and/or event
  --       log) when there is no whitespace at the end of the entry
  -- RULE: If an entry has trailing whitespace, it's siblings have the same
  --       whitespace
  -- RULE: Don't use :SCRIPT:, use org-babel
  --
  -- RULE: All TODO entries have ID and CREATED properties
  ruleTodoMustHaveIdAndCreated
  -- RULE: All TASK entries must have a tag-indicated assignment
  ruleTaskMustHaveAssignment
  -- RULE: A :LINK: tag implies a URL property, and vice versa
  ruleLinkTagMatchesUrlProperty
  -- RULE: A LINK keyword implies a :LINK: tag
  ruleLinkKeywordImpliesLinkTag
  -- RULE: An :ARCHIVE: property always refers to an existing file
  ruleArchiveTagFileExists
  -- RULE: A :FILE: tag implies an attachment, and vice versa
  ruleFileTagMatchesAttachment
  -- RULE: Category name should be no longer than 10 characters
  ruleCategoryNameCannotBeTooLong
  -- RULE: PROPERTIES drawer must be at start of entry
  rulePropertiesDrawerNeverInBody
  -- RULE: SCHEDULED, DEADLINE and other timestamps must be at start
  ruleTimestampsNeverInBody
  -- RULE: Log entries must occur before the entry body
  ruleLogEntriesNeverInBody
  -- RULE: Drawer end marker should always properly end a drawer
  ruleMisplacedDrawerEnd
  -- RULE: Check that all file links point to actual files
  ruleCheckAllLinks
  -- RULE: Log entries should never begin with a blank line
  ruleNoWhitespaceAtStartOfLogEntry
  -- RULE: No title has internal whitespace other than single spaces
  ruleNoExtraSpacesInTitle
  -- RULE: No headline is too long
  -- ruleNoOverlyLongHeadline
  -- RULE: No tag is duplicated
  ruleNoDuplicateTags
  -- RULE: No property is duplicated
  ruleNoDuplicateProperties
  -- RULE: All state changes are well ordered and flow correctly
  ruleNoInvalidStateChanges
  -- RULE: Only TODO items have SCHEDULED/DEADLINE/CLOSED timestamps
  ruleNoTimestampsOnNonTodos
  -- RULE: Only and all TODO items have NEXT/LAST_REVIEW properties
  ruleOnlyTodosReview
  -- RULE: Whitespace before and after body and log entries should match
  unless ignoreWhitespace ruleNoInconsistentWhitespace
  -- RULE: Body and log entry text should never contain only whitespace
  ruleNoEmptyBodyWhitespace
  -- RULE: No unnecessary leading or trailing whitespace
  ruleNoUnnecessaryWhitespace
  -- RULE: There should never be multiple blank lines
  ruleNoMultipleBlankLines
  -- RULE: There should be at most one logbook
  ruleAtMostOneLogBook
  -- RULE: If there is a logbook, it should contain all CLOCK entries
  ruleConsistentLogBook
  -- RULE: If there is a LOCATION, it is a valid one
  ruleLocationIsValid
  -- RULE: Plain drawers are uppercase, begin/end drawers are lowercase
  ruleDrawerCase
  -- RULE: Entries with hashes match when hashed
  ruleHashesMatch
  -- BLOG RULES: scan this entry's body for blog-strict violations (links and
  -- blocks). These activate automatically on the :posts: filetag (no flag).
  -- newartisans posts use headings, so their content reaches here as well as
  -- via the preamble. Each finding reports at the Loc of the block it was
  -- found in (report').
  when (isBlogPost org) $
    blogBodyChecks cfg mode org report' (e ^. entryBody)
 where
  inArchive = isArchive org

  ruleTodoMustHaveIdAndCreated = do
    let mkw = e ^? entryKeyword . _Just . keywordString
    when (isJust mkw || isJust (e ^? entryCategory)) $ do
      when (isNothing (e ^? property "ID")) $
        report LintWarn (TodoMissingProperty "ID")
      when (isNothing (e ^? property "CREATED")) $
        report LintWarn (TodoMissingProperty "CREATED")

  ruleTaskMustHaveAssignment = do
    let mkw = e ^? entryKeyword . _Just . keywordString
    when (mkw == Just "TASK" && null (e ^. entryTags)) $
      report LintWarn TaskMissingAssignment

  ruleLinkTagMatchesUrlProperty =
    if e ^? entryKeyword . _Just . keywordString == Just "LINK"
      then
        unless
          ("URL" `elem` e ^.. entryProperties . traverse . name)
          $ report LintWarn TodoLinkDoesNotMatchUrl
      else
        when
          ( (PlainTag "LINK" `elem` e ^. entryTags)
              /= ("URL" `elem` e ^.. entryProperties . traverse . name)
          )
          $ report LintWarn TodoLinkDoesNotMatchUrl

  ruleLinkKeywordImpliesLinkTag = do
    when
      ( (e ^? entryKeyword . _Just . keywordString == Just "LINK")
          && (PlainTag "LINK" `elem` e ^. entryTags)
      )
      $ report LintWarn TodoLinkKeywordImpliesLinkTag

  ruleArchiveTagFileExists = do
    forM_ (e ^? property "ARCHIVE") $ \path -> do
      let path' = takeWhile (/= ':') path
      unless
        ( pathExists
            cfg
            doesFileExist
            (org ^. orgFilePath)
            path'
        )
        $ report LintWarn (ArchiveTagFileDoesNotExist path')

  ruleFileTagMatchesAttachment = do
    when
      ( ("Attachments" `elem` e ^.. entryProperties . traverse . name)
          && (PlainTag "FILE" `notElem` e ^. entryTags)
      )
      $ report LintWarn TodoFileDoesNotMatchAttachment
    forM_ (e ^? property "ID") $ \ident ->
      when
        ( ( (PlainTag "FILE" `elem` e ^. entryTags)
              || ( "Attachments"
                     `elem` e ^.. entryProperties . traverse . name
                 )
          )
            && let dir =
                     (cfg ^. attachmentsDir)
                       </> take 2 ident
                       </> drop 2 ident
                in not
                     ( pathExists
                         cfg
                         doesDirectoryExist
                         (org ^. orgFilePath)
                         dir
                     )
        )
        $ report LintWarn TodoFileDoesNotMatchAttachment

  ruleCategoryNameCannotBeTooLong =
    forM_ (e ^? entryCategory) $ \cat ->
      when (length cat > 10) $
        report LintWarn (CategoryTooLong cat)

  paragraphs = bodyString (has _Paragraph)

  rulePropertiesDrawerNeverInBody =
    when
      ( any
          (=~ ("(:properties:|:PROPERTIES:)" :: String))
          paragraphs
      )
      $ report LintError MisplacedProperty

  ruleTimestampsNeverInBody =
    when
      ( any
          (=~ ("(SCHEDULED:|DEADLINE:|CLOSED:)" :: String))
          paragraphs
      )
      $ report LintError MisplacedTimestamp

  ruleLogEntriesNeverInBody =
    when
      ( any
          (=~ ("(- (CLOSING NOTE|State \"|Note taken on|Rescheduled from|Not scheduled, was|New deadline from|Removed deadline, was|Refiled on) |:LOGBOOK:|:logbook:)" :: String))
          paragraphs
      )
      $ report LintError MisplacedLogEntry

  ruleMisplacedDrawerEnd =
    when
      ( any
          (=~ ("(:end:|:END:|#\\+end|#\\+END)" :: String))
          paragraphs
      )
      $ report LintError MisplacedDrawerEnd

  ruleCheckAllLinks =
    unless (isJust (org ^? orgFileProperty "IGNORE_LINKS")) $ do
      forM_ (e ^? property "URL") $ \doc ->
        case doc
          =~ ("\\[\\[(file:|https?:)([^]:]+)" :: String) of
          AllTextSubmatches ([_, protocol, link] :: [String]) ->
            unless
              ( if protocol == "file"
                  then
                    pathExists
                      cfg
                      doesPathExist
                      (org ^. orgFilePath)
                      link
                  else level > LintAll || urlExists (protocol ++ link)
              )
              $ report
                LintError
                ( BrokenLink
                    ( if protocol == "file"
                        then link
                        else protocol ++ link
                    )
                )
          _ -> pure ()
      forM_ (e ^? property "NOTER_DOCUMENT") $ \doc ->
        case doc =~ ("([^]:]+)" :: String) of
          AllTextSubmatches ([_, link] :: [String]) ->
            unless
              ( pathExists cfg doesPathExist (org ^. orgFilePath) link
                  || "devonthink" `isInfixOf` link
              )
              $ report LintError (BrokenLink link)
          _ -> pure ()
      forM_ paragraphs $ \paragraph ->
        case paragraph
          =~ ("\\[\\[(file:|https?:)([^]:]+)" :: String) of
          AllTextSubmatches ([_, protocol, link] :: [String]) ->
            unless
              ( if protocol == "file"
                  then
                    pathExists
                      cfg
                      doesPathExist
                      (org ^. orgFilePath)
                      link
                  else level > LintAll || urlExists (protocol ++ link)
              )
              $ report
                LintError
                ( BrokenLink
                    ( if protocol == "file"
                        then link
                        else protocol ++ link
                    )
                )
          _ -> pure ()

  ruleNoWhitespaceAtStartOfLogEntry =
    forM_ (e ^.. entryLogEntries . traverse . uniplate) $ \b ->
      when
        ( case b ^? _LogBody of
            Just (Body (Whitespace _ _ : _)) -> True
            _ -> False
        )
        $ report' (b ^. _LogLoc) LintWarn WhitespaceAtStartOfLogEntry

  ruleNoExtraSpacesInTitle =
    when ("  " `isInfixOf` (e ^. entryTitle)) $
      report LintWarn TitleWithExcessiveWhitespace

  _ruleNoOverlyLongHeadline =
    when (length (e ^. entryHeadline) > (96 - e ^. entryDepth)) $
      report LintWarn OverlyLongHeadline

  ruleNoDuplicateTags =
    forM_ (findDuplicates (e ^. entryTags)) $ \tag ->
      report LintError (DuplicateTag (tag ^. tagString))

  ruleNoDuplicateProperties =
    forM_
      ( findDuplicates
          ( e
              ^.. entryProperties
                . traverse
                . name
                . to (map toLower)
          )
      )
      $ \nm ->
        report LintError (DuplicateProperty nm)

  ruleNoInvalidStateChanges = do
    (mfinalKeyword, _mfinalTime) <-
      ( \f ->
          foldM
            f
            ( Nothing
            , Nothing
            )
            -- jww (2024-05-28): Only reverse here if the configuration
            -- indicates that state entries are from most recent to least
            -- recent.
            (reverse (e ^.. entryStateHistory))
      )
        $ \(mprev, mprevTm) l -> do
          let mkwt' = l ^? _LogState . _2
              mkwf' = l ^? _LogState . _3 . _Just
              mtm = l ^? _LogTime
          forM_ ((,) <$> mtm <*> mprevTm) $ \(tm, prevTm) ->
            when (tm < prevTm) $
              report LintWarn (InvalidStateChangeWrongTimeOrder tm prevTm)
          let mkwt = fmap (^. keywordString) mkwt'
              mkwf = fmap (^. keywordString) mkwf'
              mallowed = transitionsOf cfg <$> mkwf
          unless inArchive $
            forM_ mkwf $ \kwf ->
              case mprev of
                Nothing ->
                  unless (kwf `elem` cfg ^. startKeywords) $
                    report
                      LintWarn
                      ( InvalidStateChangeInvalidTransition
                          FirstTransition
                          kwf
                          (cfg ^?! startKeywords . _head)
                      )
                Just prev ->
                  unless
                    ( prev == kwf
                        || isJust (e ^? property "LAST_REPEAT")
                    )
                    $ report
                      LintWarn
                      ( InvalidStateChangeInvalidTransition
                          IntermediateTransition
                          kwf
                          prev
                      )
          forM_ mkwt $ \kwt -> do
            if mkwf == Just kwt
              then report LintWarn (InvalidStateChangeIdempotent kwt)
              else forM_ mallowed $ \allowed ->
                unless (kwt `elem` allowed) $
                  report
                    LintWarn
                    (InvalidStateChangeTransitionNotAllowed kwt mkwf allowed)
          pure (mkwt <|> mprev, mtm <|> mprevTm)
    unless (inArchive || isJust (e ^? property "LAST_REPEAT")) $ do
      let mkw = e ^? entryKeyword . _Just . keywordString
      forM_ ((,) <$> mkw <*> mfinalKeyword) $ \(kw, finalKeyword) ->
        unless (kw == finalKeyword) $
          report
            LintWarn
            ( InvalidStateChangeInvalidTransition
                LastTransition
                kw
                finalKeyword
            )
  ruleNoTimestampsOnNonTodos =
    when
      ( any isLeadingStamp (e ^. entryStamps)
          && maybe True (not . isTodo cfg) (e ^? keyword)
      )
      $ report LintWarn TimestampsOnNonTodo

  ruleOnlyTodosReview =
    when
      ( maybe True (not . isTodo cfg) (e ^? keyword)
          && ( isJust (e ^? property "LAST_REVIEW")
                 || isJust (e ^? property "NEXT_REVIEW")
                 || isJust (e ^? property "REVIEWS")
                 || isJust (e ^? property "Effort")
             )
      )
      $ report LintWarn NonTodoWithReviewProperties

  ruleNoInconsistentWhitespace = do
    unless (consistent logLeading) $
      report LintWarn (InconsistentWhitespace "before log entries")
    unless (consistent logTrailing) $
      report LintWarn (InconsistentWhitespace "after log entries")
    when
      ( isBodyEmpty
          && isJust
            ( e
                ^? entryLogEntries
                  . _last
                  . _LogBody
                  . blocks
                  . _last
                  . _Whitespace
            )
      )
      $ report LintInfo EmptyBodyWhitespace
    unless
      ( (isLastEntry && isNothing bodyTrailing)
          || bodyLeading == bodyTrailing
      )
      $ report LintInfo (InconsistentWhitespace "surrounding body")
   where
    bodyLeading = do
      ws <- bodyWhitespace _head
      ws ^? _Whitespace . _2
    bodyTrailing = do
      ws <- bodyWhitespace _last
      ws ^? _Whitespace . _2
    bodyWhitespace f = e ^? entryBody . blocks . f
    isBodyEmpty = null (e ^. entryBody . blocks)
    logLeading = map (^? _Whitespace . _2) (logWhitespace _head)
    logTrailing =
      logTrailing'
        & _last %~ \case
          Nothing
            | isBodyEmpty -> logTrailing' ^? _head . _Just
            | Just ws <- bodyLeading -> do
                _ <- logTrailing' ^? _head . _Just
                pure ws
            | otherwise -> Nothing
          x -> x
    logTrailing' = map (^? _Whitespace . _2) (logWhitespace _last)
    logWhitespace f =
      e
        ^.. entryLogEntries
          . traverse
          . cosmos
          . _LogBody
          . blocks
          . f

  ruleNoEmptyBodyWhitespace =
    when
      ( case e ^. entryBody of
          Body [Whitespace _ _] ->
            ( e ^. entryTitle
                `elem` [ "Attending"
                       , "Agenda"
                       , "Minutes"
                       , "Notes"
                       , "Transcript"
                       ]
                && null (e ^. entryItems)
            )
              || maybe False (isTodo cfg) (e ^? keyword)
          _ -> False
      )
      $ report LintInfo EmptyBodyWhitespace

  ruleNoUnnecessaryWhitespace = do
    forM_ (e ^.. entryLogEntries . traverse . uniplate) $ \b ->
      when
        ( case b ^? _LogBody of
            Just (Body (Paragraph _ ((' ' : _) : _) : _)) ->
              True
            _ -> False
        )
        $ report' (b ^. _LogLoc) LintInfo UnnecessaryWhitespace
    when
      ( case e ^. entryBody of
          Body (Paragraph _ ((' ' : _) : _) : _) -> True
          _ -> False
      )
      $ report LintInfo UnnecessaryWhitespace

  ruleNoMultipleBlankLines =
    when (any ((> 1) . length . lines) (bodyString (has _Whitespace))) $
      report LintWarn MultipleBlankLines

  ruleAtMostOneLogBook =
    when (length (e ^.. entryLogEntries . traverse . cosmos . _LogBook) > 1) $
      report LintError MultipleLogbooks

  ruleConsistentLogBook =
    when
      ( not
          ( null
              ( e
                  ^.. entryLogEntries
                    . traverse
                    . _LogBook
                    . _2
                    . traverse
                    . filtered (hasn't _LogClock)
              )
          )
          && not
            ( null
                ( e
                    ^.. entryLogEntries
                      . traverse
                      . filtered (hasn't _LogBook)
                )
            )
      )
      $ report LintError MixedLogbooks

  ruleLocationIsValid =
    forM_ (e ^? property "LOCATION") $ \loc ->
      when (loc == "0.0,0.0") $
        report LintError (InvalidLocation loc)

  ruleDrawerCase =
    forM_
      ( e
          ^.. entryBody
            . blocks
            . traverse
            . _Drawer
            . _2
      )
      $ \drawerType ->
        unless
          ( case drawerType of
              PlainDrawer nm ->
                all (\c -> c == ':' || isUpper c) (words nm ^?! _head)
              BeginDrawer nm ->
                all
                  ( \c ->
                      c `elem` ['#', '+', '_', ':']
                        || isLower c
                  )
                  (words nm ^?! _head)
          )
          $ report LintInfo (InvalidDrawerCase drawerType)

  ruleHashesMatch =
    forM_ (e ^? property "HASH_sha512") $ \definedHash ->
      let entryWithoutHash =
            e & entryProperties %~ filter (\p -> p ^. name /= "HASH_sha512")
          actualHash = hashEntry entryWithoutHash
       in when (definedHash /= actualHash) $
            report LintWarn (HashesDoNotMatch definedHash actualHash)
   where
    hashEntry ent =
      take 64 $
        T.unpack $
          T.decodeUtf8 $
            Base16.encode $
              hash $
                T.encodeUtf8 $
                  T.pack $
                    (++ "\n") $
                      intercalate "\n" $
                        runReader (showEntry ent) cfg

  bodyString f =
    e
      ^. entryBody
        . blocks
        . traverse
        . filtered f
        . to (\b -> runReader (showBlock "" b) cfg)
      ++ e
        ^. entryLogEntries
          . traverse
          . failing (_LogState . _5) (_LogNote . _3)
          . _Just
          . blocks
          . traverse
          . filtered f
          . to (\b -> runReader (showBlock "" b) cfg)

  report' loc kind code
    | kind >= level = do
        when (level == LintDebug) $
          traceM $
            "entry: " ++ ppShow e
        tell
          [ LintMessage
              (loc ^. pos)
              kind
              code
          ]
    | otherwise = pure ()

  report = report' (e ^. entryLoc)

pathExists :: Config -> (FilePath -> IO Bool) -> FilePath -> FilePath -> Bool
pathExists cfg k path link
  | not (cfg ^. checkFiles) = True
  | otherwise = unsafePerformIO $ do
      home <- getHomeDirectory
      k
        ( if "~/" `isPrefixOf` link
            then fromMaybe home (cfg ^. homeDirectory) </> drop 2 link
            else takeDirectory path </> link
        )

urlExists :: String -> Bool
urlExists url = unsafePerformIO $ do
  (ec, _, _) <-
    readProcessWithExitCode
      "curl"
      [ "--output"
      , "/dev/null"
      , "--silent"
      , "--head"
      , "--fail"
      , "--connect-timeout"
      , "5"
      , url
      ]
      ""
  pure $ ec == ExitSuccess

showLintOrg :: FilePath -> LintMessage -> String
showLintOrg fl (LintMessage ln kind code) =
  prefix ++ " " ++ renderCode
 where
  loc = fl ++ ":" ++ show ln
  prefix = loc ++ ": " ++ renderKind
  renderKind = case kind of
    LintError -> "ERROR"
    LintWarn -> "WARN"
    LintInfo -> "INFO"
    LintAll -> "ALL"
    LintDebug -> "DEBUG"
  renderCode = case code of
    FileSlugMismatch slug ->
      "Mismatch in file slug:\ngit mv -k -- "
        ++ show fl
        ++ " "
        ++ show (fl & fileName . fileNameParts . _2 .~ slug)
    TodoMissingProperty nm ->
      "Open todo missing property " ++ show nm
    FileMissingProperty nm ->
      "File missing property " ++ show nm
    TaskMissingAssignment ->
      "Task missing assignment"
    TodoLinkDoesNotMatchUrl ->
      ":LINK: tag does not match URL property"
    TodoFileDoesNotMatchAttachment ->
      ":FILE: tag does not match presence of attachments"
    TodoLinkKeywordImpliesLinkTag ->
      "LINK keyword implies :LINK: tag, therefore it is not needed"
    ArchiveTagFileDoesNotExist path ->
      ":ARCHIVE: tag refers to a non-existent file: " ++ path
    MisplacedProperty ->
      "Misplaced :PROPERTIES: block"
    MisplacedTimestamp ->
      "Misplaced timestamp (SCHEDULED, DEADLINE or CLOSED)"
    MisplacedLogEntry ->
      "Misplaced log entry or log book"
    MisplacedDrawerEnd ->
      "Misplaced end of drawer"
    WhitespaceAtStartOfLogEntry ->
      "Log entry begins with whitespace"
    FileTitleMissing ->
      "Title is missing"
    TitleWithExcessiveWhitespace ->
      "Title with excessive whitespace"
    OverlyLongHeadline ->
      "Headline is too long"
    DuplicateFileProperty nm ->
      "Duplicated file property " ++ show nm
    DuplicateProperty nm ->
      "Duplicated property " ++ show nm
    DuplicateTag nm ->
      "Duplicated tag " ++ show nm
    DuplicatedIdentifier ident ->
      "Duplicated identifier " ++ ident
    InvalidStateChangeTransitionNotAllowed kwt mkwf allowed ->
      "Transition not allowed "
        ++ show mkwf
        ++ " -> "
        ++ show kwt
        ++ ", allowed: "
        ++ show allowed
    InvalidStateChangeInvalidTransition trans kwt kwf ->
      "Invalid "
        ++ case trans of
          FirstTransition -> "initial"
          IntermediateTransition -> "intermediate"
          LastTransition -> "final"
        ++ " state transition "
        ++ show kwf
        ++ " -> "
        ++ show kwt
    InvalidStateChangeWrongTimeOrder a b ->
      "Wrong time order in log "
        ++ show (showTime b)
        ++ " > "
        ++ show (showTime a)
    InvalidStateChangeIdempotent kw ->
      "Idempotent state transition " ++ show kw
    MultipleLogbooks ->
      "Multiple logbooks found"
    MixedLogbooks ->
      "Log entries inside and outside of logbooks found"
    TimestampsOnNonTodo ->
      "Timestamps found on non-todo entry"
    InconsistentWhitespace desc ->
      "Whitespace " ++ desc ++ " is inconsistent"
    InconsistentFilePreambleWhitespace ->
      "Whitespace surrounding file preamble is inconsistent"
    EmptyBodyWhitespace ->
      "Whitespace only body"
    UnnecessaryWhitespace ->
      "Unnecessary whitespace"
    MultipleBlankLines ->
      "Multiple blank lines"
    CategoryTooLong cat ->
      "Category name is too long: " ++ show cat
    FileCreatedTimeMismatch t1 t2 ->
      "Created time does not match file: "
        ++ show (showTime t1)
        ++ " != "
        ++ show (showTime t2)
    TitlePropertyNotLast ->
      "Title is not the last file property"
    FileTagsTodoMismatch ->
      "Filetags :todo: does not reflect todo entries in file"
    TagInFileUnknown tag ->
      "Tag in file is not part of tags vocabulary: " ++ tag
    VerbInFileUnknown tag ->
      "Verb in file is not part of verb vocabulary: " ++ tag
    InvalidLocation l ->
      "Location is not valid: " ++ l
    InvalidDrawerCase d ->
      "Drawer has invalid case: " ++ show d
    TodoMissingReviewProperties ->
      "Todo missing LAST_REVIEW and NEXT_REVIEW properties"
    NonTodoWithReviewProperties ->
      "Non-todo with LAST_REVIEW and NEXT_REVIEW properties"
    BrokenLink link ->
      "Link to missing file: " ++ link
    HashesDoNotMatch x y ->
      "Hashes do not match: " ++ x ++ " != " ++ y
    FileFailsToRoundTrip ->
      "File fails to round trip through parsing and printing"
    AudioFileNotFound path ->
      "Audio file referenced in :AUDIO: property not found: " ++ path
    BlogLegacyFileLink link ->
      "Legacy file: link in blog post, use id: or https: instead: " ++ link
    BlogUnresolvedIdLink uuid ->
      "Blog post id: link does not resolve to a known post: " ++ uuid
    BlogAbsoluteInternalLink link ->
      "Blog post uses site-absolute link: " ++ link
    BlogNonWebLinkScheme link ->
      "Blog post link uses non-web scheme (not http/https/id): " ++ link
    BlogMissingRelativeImage path ->
      "Blog post relative image not found on disk: " ++ path
    BlogRawHtmlExportBlock ->
      "Blog post has #+begin_export html block (dropped from PDF book)"
    BlogNonHtmlExportBlock backend ->
      "Blog post has non-html #+begin_export block: " ++ backend
    BlogSrcBlockOnPoetrySite ->
      "Blog post on poetry site (johnwiegley) uses #+begin_src; use verse"
    BlogVerseBlockOnCodeSite ->
      "Blog post on code site (newartisans) uses #+begin_verse; use src"
    BlogMultilineEmphasis marker snippet ->
      "Blog post inline emphasis "
        ++ show marker
        ++ " spans 3+ lines (markers leak as literal text): "
        ++ snippet
