{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Tests for the blog-strict lint rules (the @--blog@ mode).

These rules only fire when:

  1. the 'LintMode' has @_lintBlog = True@ (the @--blog@ CLI flag), AND
  2. the file's @#+filetags:@ contain the tag @posts@.

The poetry-vs-code rules additionally key off the @johnwiegley@ (poetry) vs
@newartisans@ (code) filetag.

Each rule gets at least one positive case (it fires when it should) and one
negative case (it stays silent when it should). We also prove the two gates
(mode off, and non-post file) suppress every rule, that the preamble of a
headline-less post is scanned (most johnwiegley posts have no headlines), and
that 'id:' resolution and the relative-image on-disk check work.
-}
module BlogLintTest (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Set as Set
import Org.Lint
import Org.Parse (parseOrgFile)
import Org.Types
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

-- Configuration matching the other lint test modules. _checkFiles defaults to
-- False so the filesystem-existence rules pass unless a test opts in.
lintConfig :: Config
lintConfig =
  defaultConfig
    { _openKeywords = ["TODO", "WAIT", "TASK", "LINK"]
    , _closedKeywords = ["DONE", "CANCELED"]
    , _startKeywords = ["TODO", "WAIT", "TASK"]
    , _priorities = ["A", "B", "C"]
    , _propertyColumn = 11
    , _tagsColumn = 60
    , _checkFiles = False
    }

-- The blog mode under test: --blog enabled, no known post IDs (individual
-- tests that need id: resolution build their own corpus via 'runBlogFiles').
blogMode :: LintMode
blogMode = defaultLintMode{_lintBlog = True}

-- The non-blog (default) mode, used to prove the --blog gate.
plainMode :: LintMode
plainMode = defaultLintMode

fromLines :: [ByteString] -> ByteString
fromLines = BS.intercalate "\n" . (++ [""])

parseFixture :: Config -> FilePath -> ByteString -> OrgFile
parseFixture cfg path bs =
  case parseOrgFile cfg path bs of
    Left (_, msg) ->
      error $ "test fixture must parse: " ++ msg ++ "\ninput: " ++ show bs
    Right org -> org

-- Lint a single fixture in a chosen mode at INFO level.
runBlog :: Config -> LintMode -> FilePath -> ByteString -> [LintMessage]
runBlog cfg mode path input =
  lintOrgFile cfg mode LintInfo (parseFixture cfg path input)

-- Lint several fixtures through the cross-file entry point so that id: link
-- resolution sees every file's :ID:s. Returns the flattened message list.
runBlogFiles :: Config -> LintMode -> [(FilePath, ByteString)] -> [LintMessage]
runBlogFiles cfg mode inputs =
  let parsed = map (uncurry (parseFixture cfg)) inputs
   in concat (M.elems (lintOrgFiles cfg mode LintInfo parsed))

hasCode :: (LintMessageCode -> Bool) -> [LintMessage] -> Bool
hasCode p = any (p . lintMsgCode)

shouldFire :: String -> (LintMessageCode -> Bool) -> [LintMessage] -> Assertion
shouldFire desc p msgs =
  assertBool
    ("expected " ++ desc ++ " in " ++ show (map lintMsgCode msgs))
    (hasCode p msgs)

shouldNotFire :: String -> (LintMessageCode -> Bool) -> [LintMessage] -> Assertion
shouldNotFire desc p msgs =
  assertBool
    ( "did not expect "
        ++ desc
        ++ " but got: "
        ++ show (filter (p . lintMsgCode) msgs)
    )
    (not (hasCode p msgs))

-- Predicate: any blog-prefixed code at all (used for the gating tests).
isBlogCode :: LintMessageCode -> Bool
isBlogCode = \case
  BlogLegacyFileLink _ -> True
  BlogUnresolvedIdLink _ -> True
  BlogAbsoluteInternalLink _ -> True
  BlogNonWebLinkScheme _ -> True
  BlogMissingRelativeImage _ -> True
  BlogRawHtmlExportBlock -> True
  BlogNonHtmlExportBlock _ -> True
  BlogSrcBlockOnPoetrySite -> True
  BlogVerseBlockOnCodeSite -> True
  _ -> False

-- A post header (PROPERTIES + filetags + title). The given tags string is
-- dropped into #+filetags: verbatim.
postHeader :: ByteString -> [ByteString]
postHeader filetags =
  [ ":PROPERTIES:"
  , ":ID:       POST-ID-0001"
  , ":CREATED:  [2024-10-07 Mon 20:15]"
  , ":END:"
  , "#+filetags: " <> filetags
  , "#+title: A Post"
  ]

-- A johnwiegley (poetry) post wrapping the given body lines.
poetryPost :: [ByteString] -> ByteString
poetryPost body =
  fromLines (postHeader ":johnwiegley:poetry:posts:" ++ "" : body)

-- A newartisans (code) post wrapping the given body lines.
codePost :: [ByteString] -> ByteString
codePost body =
  fromLines (postHeader ":newartisans:posts:" ++ "" : body)

-- The 'lintMsgPos' byte positions of every message whose code matches.
-- FlatParse positions count bytes remaining from the END of input, so a
-- LARGER position means CLOSER to the start of the file.
positionsOf :: (LintMessageCode -> Bool) -> [LintMessage] -> [Int]
positionsOf p msgs = [lintMsgPos m | m <- msgs, p (lintMsgCode m)]

tests :: TestTree
tests =
  testGroup
    "Blog-strict rules"
    [ linkRuleTests
    , blockRuleTests
    , siteRuleTests
    , gatingTests
    , imageTests
    , regressionTests
    ]

linkRuleTests :: TestTree
linkRuleTests =
  testGroup
    "link rules"
    [ testCase "BlogLegacyFileLink fires for [[file:...]] in preamble" $
        shouldFire
          "BlogLegacyFileLink"
          (\case BlogLegacyFileLink _ -> True; _ -> False)
          ( runBlog
              lintConfig
              blogMode
              "post.org"
              (poetryPost ["See [[file:detachment]] for more."])
          )
    , testCase "BlogLegacyFileLink silent when no file: link present" $
        shouldNotFire
          "BlogLegacyFileLink"
          (\case BlogLegacyFileLink _ -> True; _ -> False)
          ( runBlog
              lintConfig
              blogMode
              "post.org"
              (poetryPost ["See [[https://example.com][here]]."])
          )
    , testCase "BlogUnresolvedIdLink fires for unknown id: UUID" $
        shouldFire
          "BlogUnresolvedIdLink"
          (\case BlogUnresolvedIdLink _ -> True; _ -> False)
          ( runBlog
              lintConfig
              blogMode
              "post.org"
              (poetryPost ["Jump to [[id:NO-SUCH-UUID-9999]]."])
          )
    , testCase "BlogUnresolvedIdLink silent when id: resolves to a corpus ID" $
        shouldNotFire
          "BlogUnresolvedIdLink"
          (\case BlogUnresolvedIdLink _ -> True; _ -> False)
          ( runBlogFiles
              lintConfig
              blogMode
              [
                ( "linker.org"
                , poetryPost ["Jump to [[id:TARGET-ID-1234]]."]
                )
              ,
                ( "target.org"
                , fromLines
                    [ ":PROPERTIES:"
                    , ":ID:       TARGET-ID-1234"
                    , ":CREATED:  [2024-10-07 Mon 20:15]"
                    , ":END:"
                    , "#+filetags: :johnwiegley:posts:"
                    , "#+title: Target"
                    ]
                )
              ]
          )
    , testCase "BlogUnresolvedIdLink id: match is case-insensitive" $
        shouldNotFire
          "BlogUnresolvedIdLink"
          (\case BlogUnresolvedIdLink _ -> True; _ -> False)
          ( runBlogFiles
              lintConfig
              blogMode
              [
                ( "linker.org"
                , poetryPost ["Jump to [[id:target-id-abcd]]."]
                )
              ,
                ( "target.org"
                , fromLines
                    [ ":PROPERTIES:"
                    , ":ID:       TARGET-ID-ABCD"
                    , ":CREATED:  [2024-10-07 Mon 20:15]"
                    , ":END:"
                    , "#+filetags: :johnwiegley:posts:"
                    , "#+title: Target"
                    ]
                )
              ]
          )
    , testCase "BlogAbsoluteInternalLink fires for [[/abs/path]]" $
        shouldFire
          "BlogAbsoluteInternalLink"
          (\case BlogAbsoluteInternalLink _ -> True; _ -> False)
          ( runBlog
              lintConfig
              blogMode
              "post.org"
              (poetryPost ["Old link [[/2009/03/hello.html]]."])
          )
    , testCase "BlogAbsoluteInternalLink silent for https link" $
        shouldNotFire
          "BlogAbsoluteInternalLink"
          (\case BlogAbsoluteInternalLink _ -> True; _ -> False)
          ( runBlog
              lintConfig
              blogMode
              "post.org"
              (poetryPost ["Good [[https://example.com/page]]."])
          )
    , testCase "BlogNonWebLinkScheme fires for ftp: link" $
        shouldFire
          "BlogNonWebLinkScheme"
          (\case BlogNonWebLinkScheme _ -> True; _ -> False)
          ( runBlog
              lintConfig
              blogMode
              "post.org"
              (codePost ["Download [[ftp://ftp.example.com/x.tar]]."])
          )
    , testCase "BlogNonWebLinkScheme silent for http/https/id schemes" $
        shouldNotFire
          "BlogNonWebLinkScheme"
          (\case BlogNonWebLinkScheme _ -> True; _ -> False)
          ( runBlogFiles
              lintConfig
              blogMode
              [
                ( "post.org"
                , codePost
                    [ "A [[https://a.example]] and [[http://b.example]]"
                    , "and [[id:KNOWN-9]] all fine."
                    ]
                )
              ,
                ( "k.org"
                , fromLines
                    [ ":PROPERTIES:"
                    , ":ID:       KNOWN-9"
                    , ":CREATED:  [2024-10-07 Mon 20:15]"
                    , ":END:"
                    , "#+filetags: :newartisans:posts:"
                    , "#+title: K"
                    ]
                )
              ]
          )
    , testCase "BlogNonWebLinkScheme fires for irc: link" $
        shouldFire
          "BlogNonWebLinkScheme"
          (\case BlogNonWebLinkScheme _ -> True; _ -> False)
          ( runBlog
              lintConfig
              blogMode
              "post.org"
              (codePost ["Chat at [[irc://irc.example.com/chan]]."])
          )
    , testCase "file: link is not double-flagged as BlogNonWebLinkScheme" $
        -- file: links get the dedicated BlogLegacyFileLink Error; they must
        -- NOT additionally trip the generic non-web-scheme warning.
        shouldNotFire
          "BlogNonWebLinkScheme"
          (\case BlogNonWebLinkScheme _ -> True; _ -> False)
          ( runBlog
              lintConfig
              blogMode
              "post.org"
              (poetryPost ["See [[file:detachment]] here."])
          )
    ]

blockRuleTests :: TestTree
blockRuleTests =
  testGroup
    "export block rules"
    [ testCase "BlogRawHtmlExportBlock fires for html block that is not <!--more-->" $
        shouldFire
          "BlogRawHtmlExportBlock"
          (== BlogRawHtmlExportBlock)
          ( runBlog
              lintConfig
              blogMode
              "post.org"
              ( poetryPost
                  [ "#+begin_export html"
                  , "<table class=\"persian\"><tr><td>x</td></tr></table>"
                  , "#+end_export"
                  ]
              )
          )
    , testCase "BlogRawHtmlExportBlock silent for the <!--more--> teaser" $
        shouldNotFire
          "BlogRawHtmlExportBlock"
          (== BlogRawHtmlExportBlock)
          ( runBlog
              lintConfig
              blogMode
              "post.org"
              ( poetryPost
                  [ "Intro paragraph."
                  , "#+begin_export html"
                  , "<!--more-->"
                  , "#+end_export"
                  , "More text."
                  ]
              )
          )
    , testCase "BlogNonHtmlExportBlock fires for #+begin_export latex" $
        shouldFire
          "BlogNonHtmlExportBlock"
          (\case BlogNonHtmlExportBlock _ -> True; _ -> False)
          ( runBlog
              lintConfig
              blogMode
              "post.org"
              ( codePost
                  [ "#+begin_export latex"
                  , "\\newpage"
                  , "#+end_export"
                  ]
              )
          )
    , testCase "BlogNonHtmlExportBlock silent when only an html export exists" $
        shouldNotFire
          "BlogNonHtmlExportBlock"
          (\case BlogNonHtmlExportBlock _ -> True; _ -> False)
          ( runBlog
              lintConfig
              blogMode
              "post.org"
              ( poetryPost
                  [ "#+begin_export html"
                  , "<!--more-->"
                  , "#+end_export"
                  ]
              )
          )
    , testCase "export rules also fire from an entry body (not just preamble)" $
        shouldFire
          "BlogNonHtmlExportBlock under a headline"
          (\case BlogNonHtmlExportBlock _ -> True; _ -> False)
          ( runBlog
              lintConfig
              blogMode
              "post.org"
              ( fromLines
                  ( postHeader ":newartisans:posts:"
                      ++ [ ""
                         , "* A section"
                         , ""
                         , "#+begin_export latex"
                         , "\\clearpage"
                         , "#+end_export"
                         ]
                  )
              )
          )
    ]

siteRuleTests :: TestTree
siteRuleTests =
  testGroup
    "poetry-vs-code site rules"
    [ testCase "BlogSrcBlockOnPoetrySite fires for #+begin_src on johnwiegley" $
        shouldFire
          "BlogSrcBlockOnPoetrySite"
          (== BlogSrcBlockOnPoetrySite)
          ( runBlog
              lintConfig
              blogMode
              "post.org"
              ( poetryPost
                  [ "#+begin_src haskell"
                  , "main = pure ()"
                  , "#+end_src"
                  ]
              )
          )
    , testCase "BlogSrcBlockOnPoetrySite silent for #+begin_src on newartisans" $
        shouldNotFire
          "BlogSrcBlockOnPoetrySite"
          (== BlogSrcBlockOnPoetrySite)
          ( runBlog
              lintConfig
              blogMode
              "post.org"
              ( codePost
                  [ "#+begin_src haskell"
                  , "main = pure ()"
                  , "#+end_src"
                  ]
              )
          )
    , testCase "BlogVerseBlockOnCodeSite fires for #+begin_verse on newartisans" $
        shouldFire
          "BlogVerseBlockOnCodeSite"
          (== BlogVerseBlockOnCodeSite)
          ( runBlog
              lintConfig
              blogMode
              "post.org"
              ( codePost
                  [ "#+begin_verse"
                  , "A line of code-site poetry"
                  , "#+end_verse"
                  ]
              )
          )
    , testCase "BlogVerseBlockOnCodeSite silent for #+begin_verse on johnwiegley" $
        shouldNotFire
          "BlogVerseBlockOnCodeSite"
          (== BlogVerseBlockOnCodeSite)
          ( runBlog
              lintConfig
              blogMode
              "post.org"
              ( poetryPost
                  [ "#+begin_verse"
                  , "A line of real poetry"
                  , "#+end_verse"
                  ]
              )
          )
    , testCase "site rules silent when no site tag present" $
        shouldNotFire
          "any site rule"
          (\c -> c == BlogSrcBlockOnPoetrySite || c == BlogVerseBlockOnCodeSite)
          ( runBlog
              lintConfig
              blogMode
              "post.org"
              ( fromLines
                  [ ":PROPERTIES:"
                  , ":ID:       POST-ID-XYZ"
                  , ":CREATED:  [2024-10-07 Mon 20:15]"
                  , ":END:"
                  , "#+filetags: :posts:"
                  , "#+title: No site tag"
                  , ""
                  , "#+begin_src haskell"
                  , "x = 1"
                  , "#+end_src"
                  , "#+begin_verse"
                  , "poem"
                  , "#+end_verse"
                  ]
              )
          )
    ]

gatingTests :: TestTree
gatingTests =
  testGroup
    "gating"
    [ testCase "no blog rule fires when --blog is off (plain mode)" $
        shouldNotFire
          "any blog code"
          isBlogCode
          ( runBlog
              lintConfig
              plainMode
              "post.org"
              ( poetryPost
                  [ "See [[file:detachment]] and [[/abs/x.html]]"
                  , "and [[ftp://h/x]] and [[id:NOPE-1]]."
                  , "#+begin_src haskell"
                  , "x = 1"
                  , "#+end_src"
                  , "#+begin_export latex"
                  , "\\x"
                  , "#+end_export"
                  ]
              )
          )
    , testCase "no blog rule fires when file is not tagged :posts:" $
        shouldNotFire
          "any blog code"
          isBlogCode
          ( runBlog
              lintConfig
              blogMode
              "notes.org"
              ( fromLines
                  [ ":PROPERTIES:"
                  , ":ID:       NOTE-ID-1"
                  , ":CREATED:  [2024-10-07 Mon 20:15]"
                  , ":END:"
                  , "#+filetags: :johnwiegley:poetry:"
                  , "#+title: Just notes, not a post"
                  , ""
                  , "See [[file:detachment]] and [[ftp://h/x]]."
                  , "#+begin_src haskell"
                  , "x = 1"
                  , "#+end_src"
                  ]
              )
          )
    , testCase "headline-less preamble-only post is scanned" $
        -- Proves preamble scanning: this post has NO headlines at all, so its
        -- only content is the file preamble. The file: link must still fire.
        shouldFire
          "BlogLegacyFileLink (preamble-only)"
          (\case BlogLegacyFileLink _ -> True; _ -> False)
          ( runBlog
              lintConfig
              blogMode
              "preamble-only.org"
              ( fromLines
                  [ ":PROPERTIES:"
                  , ":ID:       PRE-ONLY-1"
                  , ":CREATED:  [2024-10-07 Mon 20:15]"
                  , ":END:"
                  , "#+filetags: :johnwiegley:poetry:posts:"
                  , "#+title: Preamble only"
                  , ""
                  , "All my content lives here, with [[file:soul]] inside."
                  ]
              )
          )
    ]

imageTests :: TestTree
imageTests =
  testGroup
    "relative image existence"
    [ testCase "BlogMissingRelativeImage fires for missing [[./img]]" $
        shouldFire
          "BlogMissingRelativeImage"
          (\case BlogMissingRelativeImage _ -> True; _ -> False)
          ( runBlog
              (lintConfig{_checkFiles = True})
              blogMode
              "/tmp/blogtest-missing/post.org"
              (codePost ["Figure: [[./images/missing.png]]."])
          )
    , testCase "BlogMissingRelativeImage silent when the image exists on disk" $
        withSystemTempDirectory "blog-image" $ \dir -> do
          let imgDir = dir </> "images"
          createDirectoryIfMissing True imgDir
          BS.writeFile (imgDir </> "real.png") "fake png bytes"
          let post = dir </> "post.org"
              msgs =
                runBlog
                  (lintConfig{_checkFiles = True})
                  blogMode
                  post
                  (codePost ["Figure: [[./images/real.png]]."])
          shouldNotFire
            "BlogMissingRelativeImage"
            (\case BlogMissingRelativeImage _ -> True; _ -> False)
            msgs
    , testCase "BlogMissingRelativeImage fires for missing file:images/ image" $
        shouldFire
          "BlogMissingRelativeImage (file:images/)"
          (\case BlogMissingRelativeImage _ -> True; _ -> False)
          ( runBlog
              (lintConfig{_checkFiles = True})
              blogMode
              "/tmp/blogtest-missing2/post.org"
              (poetryPost ["Old image [[file:images/patbunny.jpg]]."])
          )
    , testCase "blogMode and plainMode differ in _lintBlog; default ids empty" $ do
        _lintBlog blogMode @?= True
        _lintBlog plainMode @?= False
        Set.size (_lintPostIds defaultLintMode) @?= 0
    ]

-- Regression tests for the three defects found by running --blog against the
-- real johnwiegley + newartisans corpus.
regressionTests :: TestTree
regressionTests =
  testGroup
    "corpus regressions"
    [ testCase "indented <!--more--> export block is exempt (johnwiegley style)" $
        -- Defect 2: johnwiegley indents its teaser two spaces. After
        -- whitespace normalization it must still be exempt, like the
        -- flush-left newartisans form.
        shouldNotFire
          "BlogRawHtmlExportBlock"
          (== BlogRawHtmlExportBlock)
          ( runBlog
              lintConfig
              blogMode
              "post.org"
              ( poetryPost
                  [ "Intro."
                  , "#+begin_export html"
                  , "  <!--more-->"
                  , "#+end_export"
                  , "Body."
                  ]
              )
          )
    , testCase "real <table> export block still fires despite indentation" $
        -- The fix must not over-exempt: a block with actual HTML content is
        -- still flagged (this is the turn-thereunto case).
        shouldFire
          "BlogRawHtmlExportBlock"
          (== BlogRawHtmlExportBlock)
          ( runBlog
              lintConfig
              blogMode
              "post.org"
              ( poetryPost
                  [ "#+begin_export html"
                  , "  <table class=\"persian\">"
                  , "    <tr><td>x</td></tr>"
                  , "  </table>"
                  , "#+end_export"
                  ]
              )
          )
    , testCase "mailto: link is NOT flagged as non-web scheme" $
        -- Defect 1: mailto: works on both the website and the PDF book.
        shouldNotFire
          "BlogNonWebLinkScheme"
          (\case BlogNonWebLinkScheme _ -> True; _ -> False)
          ( runBlog
              lintConfig
              blogMode
              "post.org"
              (codePost ["Mail me at [[mailto:johnw@newartisans.com]]."])
          )
    , testCase "ftp:/irc: still flagged when a mailto: link is also present" $
        -- The mailto allowance must be narrow: other obscure schemes in the
        -- same post still fire.
        shouldFire
          "BlogNonWebLinkScheme"
          (\case BlogNonWebLinkScheme _ -> True; _ -> False)
          ( runBlog
              lintConfig
              blogMode
              "post.org"
              ( codePost
                  [ "Mail [[mailto:johnw@newartisans.com]],"
                  , "download [[ftp://ftp.dead/x]], chat [[irc://h/c]]."
                  ]
              )
          )
    , testCase "preamble file: link reports its own block, not end-of-preamble" $ do
        -- Defect 3: a finding in a long headline-less preamble must report at
        -- the byte position of the block containing it, not a single fixed
        -- (end-of-preamble) position. We place a file: link near the TOP and
        -- an ftp: link near the BOTTOM of a long preamble. FlatParse positions
        -- count from the end, so the top link must have a STRICTLY LARGER
        -- position than the bottom link. Before the fix both shared one
        -- position and would have been equal.
        let body =
              ["Top has [[file:hard.purpose]] link near the start."]
                ++ [""]
                ++ replicate 40 "Filler paragraph line that lengthens the body."
                ++ [""]
                ++ ["Bottom has [[ftp://host/file]] link near the end."]
            msgs =
              runBlog
                lintConfig
                blogMode
                "long-preamble.org"
                (poetryPost body)
            topPos =
              positionsOf (\case BlogLegacyFileLink _ -> True; _ -> False) msgs
            botPos =
              positionsOf (\case BlogNonWebLinkScheme _ -> True; _ -> False) msgs
        -- Both findings are present.
        assertBool "expected a top file: finding" (not (null topPos))
        assertBool "expected a bottom ftp: finding" (not (null botPos))
        -- Top finding is nearer the start than the bottom finding.
        assertBool
          ( "top file: link should report nearer start than bottom ftp: link; "
              ++ "got top="
              ++ show topPos
              ++ " bottom="
              ++ show botPos
          )
          (maximum topPos > maximum botPos)
    ]
