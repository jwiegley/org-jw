{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# HLINT ignore "Use lambda-case" #-}

module Org.Site where

import Control.Applicative
import Control.Arrow ((***))
import Control.Monad hiding (forM_)
import Data.Aeson (FromJSON (..), Value (Object), (.:))
import qualified Data.Aeson.Key as AT
import qualified Data.Aeson.KeyMap as AT
import Data.Aeson.Types (typeMismatch)
import qualified Data.Aeson.Types as AT
import Data.Char (toLower)
import Data.Foldable hiding (elem)
import Data.Functor
import Data.Generics (everywhereM, mkM)
import Data.List hiding (all, any, concatMap)
import Data.List.Split hiding (oneOf)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time
import Data.Time.Format.ISO8601
import Data.Yaml (decodeFileEither)
import Hakyll
import Hakyll.Images (loadImage, resizeImageCompiler)
import System.Directory
import System.FilePath
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcess)
import qualified Text.Pandoc as P
import Text.Regex.Posix hiding (empty, match)
import Prelude hiding (all, any, concatMap)

siteRules :: UTCTime -> SiteConfiguration -> Rules ()
siteRules now site@SiteConfiguration {..} = do
  match "templates/*" $
    compile templateCompiler

  match
    ( "files/**"
        .||. "favicon.ico"
    )
    $ do
      route idRoute
      compile copyFileCompiler

  match "images/**" $ do
    route idRoute
    compile $
      loadImage
        >>= resizeImageCompiler 1024 768

  match ("css/*.css" .||. "js/*.js") $ do
    route idRoute
    compile yuiCompressor

  tags <-
    buildTagsWith
      (getTagsByField "tags")
      "posts/**.org"
      (fromCapture "tags/*/index.html")

  posts <- getMatchesToPublishBefore now "posts/*.org"
  match (fromList posts) $ do
    route $ metadataRoute (constRoute . getRouteFromMeta)
    compile $
      postPandocCompiler posts
        >>= saveSnapshot "teaser"
        >>= "templates/post.html"
          $$= postCtxWithTags tags
        >>= saveSnapshot "content"
        >>= loadForSite

  match ("posts/**.jpg" .||. "posts/**.png") $ do
    -- For images, remove the "posts/" prefix
    route $
      customRoute
        ( joinPath
            . drop 1
            . splitDirectories
            . toFilePath
        )
    compile $
      loadImage
        >>= resizeImageCompiler 1024 768

  tagsRules tags $ \tag pat -> do
    route idRoute
    compile $ do
      ps <- recentFirst =<< loadAll pat
      makeItem ""
        >>= "templates/archives.html"
          $$= ( constField "title" ("Posts tagged \"" ++ tag ++ "\"")
                  <> listField "posts" (postCtxWithTags tags) (return ps)
                  <> listField
                    "tags"
                    postCtx
                    ( return $
                        map
                          (\x -> Item (fromFilePath (fst x)) (fst x))
                          (tagsMap tags)
                    )
                  <> defaultContext
              )
        >>= loadForSite

  paginate posts 6 10 $ \idx maxIndex itemsForPage ->
    create
      [ fromFilePath $
          if idx == 1
            then "index.html"
            else "page/" ++ show idx ++ "/index.html"
      ]
      $ do
        route idRoute
        compile $
          makeItem ""
            >>= "templates/list.html"
              $$= ( listField
                      "posts"
                      (field "teaser" teaserBody <> postCtxWithTags tags)
                      ( forM itemsForPage $ \ident ->
                          loadSnapshot ident "teaser"
                            >>= wordpressifyUrls
                            >>= relativizeUrls
                      )
                      <> ( if idx == 1
                             then constField "isFirst" "true"
                             else mempty
                         )
                      <> ( if idx == 2
                             then constField "isSecond" "true"
                             else mempty
                         )
                      <> ( if idx == maxIndex
                             then constField "isLast" "true"
                             else mempty
                         )
                      <> constField "nextIndex" (show (succ idx))
                      <> constField "prevIndex" (show (pred idx))
                      <> defaultContext
                  )
            >>= loadForSite

  create ["archives/index.html"] $ do
    route idRoute
    compile $
      makeItem ""
        >>= "templates/archives.html"
          $$= ( listField
                  "posts"
                  (postCtxWithTags tags)
                  ( forM posts $ \post ->
                      loadSnapshot post "teaser"
                        >>= wordpressifyUrls
                        >>= relativizeUrls
                  )
                  <> listField
                    "tags"
                    postCtx
                    ( return $
                        map (\x -> Item (fromFilePath (fst x)) (fst x)) $
                          tagsMap tags
                    )
                  <> defaultContext
              )
        >>= loadForSite

  pages <- getMatches "pages/*.org"
  match (fromList pages) $ do
    route $ metadataRoute (constRoute . getRouteFromMeta)
    compile $
      postPandocCompiler pages
        >>= "templates/page.html" $$= defaultContext
        >>= loadForSite
        >>= wordpressifyUrls
        >>= relativizeUrls

  create ["atom.xml"] $ do
    route idRoute
    compile $
      renderAtom
        (feedConfigurationFromSite site)
        ( postCtxWithTags tags
            <> feedContext siteRoot
        )
        . take 10
        =<< recentFirst
        =<< traverse (`loadSnapshot` "content") posts

  create ["rss.xml"] $ do
    route idRoute
    compile
      ( renderRss
          (feedConfigurationFromSite site)
          ( postCtxWithTags tags
              <> feedContext siteRoot
          )
          . take 10
          =<< recentFirst
          =<< traverse (`loadSnapshot` "content") posts
      )

  create ["robots.txt"] $ do
    route idRoute
    compile $ do
      makeItem ("" :: String)
        >>= "templates/robots.txt" $$= (siteCtx site <> defaultContext)

  create ["sitemap.xml"] $ do
    route idRoute
    compile $ do
      pos <- recentFirst =<< traverse load posts
      pas <- loadAll "pages/*.org"
      makeItem ("" :: String)
        >>= "templates/sitemap.xml"
          $$= ( listField
                  "entries"
                  postCtx
                  (return $ pos ++ pas)
                  <> siteCtx site
                  <> defaultContext
              )
  where
    loadForSite =
      "templates/meta.html"
        $$= (siteCtx site <> defaultContext)
        >=> wordpressifyUrls
        >=> relativizeUrls

    postCtx :: Context String
    postCtx =
      mconcat
        [ dateField "date" "%B %e, %Y",
          dateField "year" "%Y",
          dateField "mon" "%m",
          dateField "month" "%B",
          dateField "day_" "%d",
          dateField "day" "%e",
          wpIdentField "ident",
          wpUrlField "url",
          metadataField,
          siteCtx site,
          defaultContext
        ]

    postCtxWithTags :: Tags -> Context String
    postCtxWithTags tags = tagsField "tags" tags <> postCtx

    postPandocCompiler :: [Identifier] -> Compiler (Item String)
    postPandocCompiler entries = do
      ident <- getUnderlying
      pandocCompilerWithTransformM
        defaultHakyllReaderOptions
        ( defaultHakyllWriterOptions
            { P.writerTableOfContents = True,
              P.writerListings = True,
              P.writerTOCDepth = 2
            }
        )
        (unsafeCompiler . fixPostLinks ident)
      where
        fixPostLinks :: Identifier -> P.Pandoc -> IO P.Pandoc
        fixPostLinks ident = everywhereM (mkM (fixPostLink ident))

        -- Fixup Org-roam links to refer the intended document by its route.
        fixPostLink :: Identifier -> P.Inline -> IO P.Inline
        fixPostLink _ident l@(P.Link as title (T.unpack -> url, title'))
          | AllTextSubmatches [_, uuid] <- url =~ ("^id:(.+)$" :: String) = do
              -- Within the [Identifier] gives by entries, find one whose
              -- metadata id == uuid.
              findEntryByUuid entries uuid <&> \case
                Nothing -> l
                Just path ->
                  P.Link
                    as
                    title
                    (T.pack ("/" ++ path), title')
        fixPostLink ident (P.Image as title (T.unpack -> url, title'))
          | AllTextSubmatches [_, target] <-
              url
                =~ ( "^\\./("
                       ++ dropExtension (takeBaseName (toFilePath ident))
                       ++ "/.*)$" ::
                       String
                   ) =
              pure $
                P.Image
                  as
                  title
                  (T.pack ("/" ++ target), title')
        fixPostLink _ x = return x

{------------------------------------------------------------------------}
-- Main code

mapMaybeM :: (Applicative m) => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f = foldr g (pure [])
  where
    g a = liftA2 (maybe id (:)) (f a)

($$=) :: Identifier -> Context a -> Item a -> Compiler (Item String)
($$=) = loadAndApplyTemplate

yuiCompressor :: Compiler (Item String)
yuiCompressor = do
  path <- getResourceFilePath
  makeItem $ unsafePerformIO $ readProcess "yuicompressor" [path] ""

{------------------------------------------------------------------------}
-- Site configuration

data SiteConfiguration = SiteConfiguration
  { siteTitle :: String, -- Title of the site
    siteDescription :: String, -- Description of the site
    siteAuthorName :: String, -- Name of the site author
    siteAuthorEmail :: String, -- Email of the site author
    siteRoot :: String, -- Root URI of the site
    siteName :: String, -- If the site at foo.com, then foo
    siteDeploy :: String, -- Deploy command, replace %s with name
    siteKeywords :: String, -- Site keywords
    siteCopyright :: String, -- Site copyright
    siteAnalytics :: String, -- Google Analytics Id
    siteDisqus :: String, -- Disqus domainname
    siteContentDir :: String, -- Path to pages, posts, drafts
    siteDir :: String -- Path to templates and other files
  }

instance FromJSON SiteConfiguration where
  parseJSON (Object v) =
    SiteConfiguration
      <$> v .: "title"
      <*> v .: "description"
      <*> v .: "authorName"
      <*> v .: "authorEmail"
      <*> v .: "root"
      <*> v .: "name"
      <*> v .: "deploy"
      <*> v .: "keywords"
      <*> v .: "copyright"
      <*> v .: "analytics"
      <*> v .: "disqus"
      <*> v .: "contentDir"
      <*> v .: "siteDir"
  parseJSON invalid = typeMismatch "SiteConfiguration" invalid

readSiteConfiguration :: FilePath -> IO SiteConfiguration
readSiteConfiguration file = do
  eres <- decodeFileEither file
  case eres of
    Left err ->
      error $
        "Could not open or parse "
          ++ file
          ++ " file: "
          ++ show err
    Right conf -> pure conf

feedConfigurationFromSite :: SiteConfiguration -> FeedConfiguration
feedConfigurationFromSite SiteConfiguration {..} =
  FeedConfiguration
    { feedTitle = siteTitle,
      feedDescription = siteDescription,
      feedAuthorName = siteAuthorName,
      feedAuthorEmail = siteAuthorEmail,
      feedRoot = siteRoot
    }

siteCtx :: SiteConfiguration -> Context String
siteCtx SiteConfiguration {..} =
  mconcat
    [ constField "title" siteTitle,
      constField "description" siteDescription,
      constField "authorName" siteAuthorName,
      constField "authorEmail" siteAuthorEmail,
      constField "root" siteRoot,
      constField "name" siteName,
      constField "deploy" siteDeploy,
      constField "keywords" siteKeywords,
      constField "copyright" siteCopyright,
      constField "analytics" siteAnalytics,
      constField "disqus" siteDisqus,
      constField "contentDir" siteContentDir,
      constField "siteDir" siteDir
    ]

{------------------------------------------------------------------------}
-- Content normalization

teaserBody :: Item String -> Compiler String
teaserBody =
  return . extractTeaser . maxLengthTeaser . compactTeaser . itemBody
  where
    extractTeaser [] = []
    extractTeaser xs@(x : xr)
      | "<!--more-->" `isPrefixOf` xs = []
      | otherwise = x : extractTeaser xr

    maxLengthTeaser s
      | isNothing (findIndex (isPrefixOf "<!--more-->") (tails s)) =
          unwords (take 60 (words s))
      | otherwise = s

    compactTeaser =
      replaceAll "<iframe [^>]*>" (const "")
        . replaceAll "<img [^>]*>" (const "")
        . replaceAll "<p>" (const "")
        . replaceAll "</p>" (const "")
        . replaceAll "<br />" (const "<br/>")
        . replaceAll "<div class=\"line-block\">" (const "")
        . replaceAll "</div>" (const "")
        . replaceAll "<blockquote>" (const "")
        . replaceAll "</blockquote>" (const "")
        . replaceAll "<strong>" (const "")
        . replaceAll "</strong>" (const "")
        . replaceAll "<ol>" (const "")
        . replaceAll "</ol>" (const "")
        . replaceAll "<ul>" (const "")
        . replaceAll "</ul>" (const "")
        . replaceAll "<li>" (const "")
        . replaceAll "</li>" (const "")
        . replaceAll "<h[0-9][^>]*>" (const "")
        . replaceAll "</h[0-9]>" (const "")
        . replaceAll "<pre[^>]*>" (const "")
        . replaceAll "</pre>" (const "")
        . replaceAll "<a [^>]*>" (const "")
        . replaceAll "</a>" (const "")

wordpressifyUrls :: Item String -> Compiler (Item String)
wordpressifyUrls item = do
  rt <- getRoute (itemIdentifier item)
  return $ case rt of
    Nothing -> item
    Just _ -> wordpressifyUrlsWith <$> item
  where
    wordpressifyUrlsWith = withUrls $ replaceAll "/index.html" (const "/")

wpUrlField :: String -> Context String
wpUrlField key =
  field key $
    fmap (maybe "" toWordPressUrl) . getRoute . itemIdentifier
  where
    toWordPressUrl = replaceAll "/index.html" (const "/") . toUrl

wpIdentField :: String -> Context String
wpIdentField = mapContext (last . init . splitOn "/") . wpUrlField

{------------------------------------------------------------------------}
-- Sorting and pagination

getMatchesToPublishBefore ::
  (MonadMetadata m) =>
  UTCTime ->
  Pattern ->
  m [Identifier]
getMatchesToPublishBefore moment pat = do
  getMatches pat
    >>= mapMaybeM (\i -> fmap (,i) <$> itemUTC defaultTimeLocale i)
    <&> reverse
      . map snd
      . sortOn fst
      . filter
        ( \(date, _) ->
            -- Only posts intended to be published have the "published"
            -- metadata field; this requires the Org file be tagged with
            -- :publish=NAME: and that it have a :CREATED: or :PUBLISH: date
            -- in its properties.
            diffUTCTime date moment < 0
        )

paginate ::
  [Identifier] ->
  Int ->
  Int ->
  (Int -> Int -> [Identifier] -> Rules ()) ->
  Rules ()
paginate idents itemsPerPage pageLimit rules =
  zipWithM_ process pageNumbers chunks
  where
    chunks = take pageLimit $ chunksOf itemsPerPage idents
    maxIndex = length chunks
    pageNumbers = take maxIndex [1 ..]
    process i = rules i maxIndex

{------------------------------------------------------------------------}
-- RSS/Atom feed

feedContext :: String -> Context String
feedContext root =
  mconcat
    [ rssTitleField "title",
      rssBodyField root "description"
    ]

rssTitleField :: String -> Context String
rssTitleField key = field key $ \i -> do
  value <- getMetadataField (itemIdentifier i) "title"
  maybe empty (return . replaceAll "&" (const "&amp;")) value

rssBodyField :: String -> String -> Context String
rssBodyField root key =
  field key $
    return
      . replaceAll "<iframe [^>]*>" (const "")
      . withUrls wordpress
      . withUrls absolute
      . itemBody
  where
    wordpress = replaceAll "/index.html" (const "/")

    absolute x@('/' : _) = root ++ x
    absolute x = x

pandocMathCompiler :: Compiler (Item String)
pandocMathCompiler =
  let mathExtensions =
        [ P.Ext_tex_math_dollars,
          P.Ext_tex_math_double_backslash,
          P.Ext_latex_macros
        ]
      defaultExtensions = P.writerExtensions defaultHakyllWriterOptions
      newExtensions = foldr P.enableExtension defaultExtensions mathExtensions
      writerOptions =
        defaultHakyllWriterOptions
          { P.writerExtensions = newExtensions,
            P.writerHTMLMathMethod = P.MathJax ""
          }
   in pandocCompilerWith defaultHakyllReaderOptions writerOptions

{------------------------------------------------------------------------}
-- Metadata

pandocMetadata :: Maybe String -> FilePath -> IO Metadata
pandocMetadata mname file = do
  P.Pandoc (P.Meta meta) blocks <- do
    cnt <- TIO.readFile file
    case P.runPure $ P.readOrg P.def cnt of
      Right t -> return t
      Left e -> error $ "Pandoc read failed: " ++ show e
  let furtherMeta =
        M.fromList $
          mapMaybe
            ( \case
                -- Properties at file scope like "#+filetags: TAGS" are not
                -- processed as metadata by the default Pandoc reader. So we
                -- scan through for RawBlocks that match this pattern and read
                -- them as tags here.
                P.RawBlock (P.Format "org") (T.unpack -> text) ->
                  case text =~ ("#\\+([A-Za-z]+):[ \t]+(.+)" :: String) of
                    AllTextSubmatches [_, key, value] ->
                      Just (T.pack key, P.MetaString (T.pack value))
                    _ -> Nothing
                _ -> Nothing
            )
            blocks
      -- The 'Semigroup' operation for 'Map' is 'union', which prefers values
      -- from the left operand.
      metadata = cleanupMetadata mname (meta <> furtherMeta)
  pure $ buildMetadata file (P.Meta metadata)

buildMetadata :: FilePath -> P.Meta -> Metadata
buildMetadata file meta@(P.Meta metadata) =
  AT.fromList $
    map (AT.fromString *** AT.String) $
      filter (not . T.null . snd) $
        map (\(f, ex, wr) -> (f, inlinesTo wr (ex meta))) $
          [ ("published", publishDate, P.writePlain),
            ("edited", editedDate file, P.writePlain),
            ("route", publishRoute, P.writePlain),
            ("titleHtml", metaField "title", P.writeHtml5String)
          ]
            ++ M.foldMapWithKey
              (\k _ -> [(T.unpack k, metaField k, P.writePlain)])
              metadata

cleanupMetadata ::
  Maybe String ->
  M.Map T.Text P.MetaValue ->
  M.Map T.Text P.MetaValue
cleanupMetadata mname = M.foldMapWithKey ((M.fromList .) . go)
  where
    go "filetags" (P.MetaString value) =
      [ ( "shouldPublish",
          P.MetaBool
            (T.unpack value =~ (":publish=" ++ name ++ ":" :: String))
        )
        | Just name <- [mname]
      ]
        ++ [ ( "tags",
               P.MetaString
                 ( T.intercalate ", "
                     . filter
                       ( \(T.unpack -> s) ->
                           not (s =~ ("^publish=" :: String))
                       )
                     . filter (not . T.null)
                     . T.splitOn ":"
                     $ value
                 )
             )
           ]
    go key value = [(key, value)]

publishRoute :: P.Meta -> [P.Inline]
publishRoute meta =
  datePath
    ++ slugPath
    ++ [P.Str "index.html"]
  where
    slugPath = metaField "slug" meta ++ [P.Str "/"]
    datePath =
      case T.unpack (stringify (publishDate meta))
        =~ ("([0-9]+)-([0-9]+)-" :: String) of
        AllTextSubmatches [_, year, month] ->
          [ P.Str (T.pack year),
            P.Str "/",
            P.Str (T.pack month),
            P.Str "/"
          ]
        _ -> []

getRouteFromMeta :: Metadata -> FilePath
getRouteFromMeta meta =
  case lookupString "route" meta of
    Nothing -> error $ "missing route: " ++ show meta
    Just rte -> rte

publishDateOrDocDate :: P.Meta -> [P.Inline]
publishDateOrDocDate meta =
  case publishDate meta of
    [] -> P.docDate meta
    xs -> xs

publishDate :: P.Meta -> [P.Inline]
publishDate meta =
  case P.lookupMeta "shouldPublish" meta of
    Just (P.MetaBool True) ->
      case metaField "publish" meta of
        [P.Str s] | Just date <- orgDateToIso s -> [P.Str date]
        _ ->
          case metaField "created" meta of
            [P.Str s] | Just date <- orgDateToIso s -> [P.Str date]
            _ -> []
    _ -> []

itemUTC ::
  (MonadMetadata m) =>
  TimeLocale ->
  Identifier ->
  m (Maybe UTCTime)
itemUTC locale ident =
  getMetadataField ident "published"
    <&> (>>= parseTimeM True locale "%Y-%m-%d")

editedDate :: FilePath -> P.Meta -> [P.Inline]
editedDate file meta =
  case metaField "edited" meta of
    [P.Str s] | Just date <- orgDateToIso s -> [P.Str date]
    _ -> case publishDateOrDocDate meta of
      [] -> unsafePerformIO $ do
        lastModified <- getModificationTime file
        pure [P.Str (T.pack (formatShow iso8601Format (utctDay lastModified)))]
      date -> date

findEntryByUuid :: [Identifier] -> String -> IO (Maybe FilePath)
findEntryByUuid entries (map toLower -> uuid) = do
  firstMatching entries $ \entry -> do
    let path = toFilePath entry
    if takeExtension path == ".org"
      then hasUuid <$> pandocMetadata Nothing path
      else pure Nothing
  where
    -- If this post has the uuid we're looking for, return its route.
    hasUuid :: Metadata -> Maybe FilePath
    hasUuid meta = case lookupString "id" meta of
      Just (map toLower -> postUuid)
        | uuid == postUuid -> lookupString "route" meta
      _ -> Nothing

metaField :: T.Text -> P.Meta -> [P.Inline]
metaField name meta =
  case P.lookupMeta name meta of
    Just (P.MetaString s) -> [P.Str s]
    Just (P.MetaInlines ils) -> ils
    Just (P.MetaBlocks [P.Plain ils]) -> ils
    Just (P.MetaBlocks [P.Para ils]) -> ils
    _ -> []

firstMatching :: (Monad m) => [a] -> (a -> m (Maybe b)) -> m (Maybe b)
firstMatching [] _ = pure Nothing
firstMatching (x : xs) f =
  f x >>= \case
    Nothing -> firstMatching xs f
    res -> pure res

inlinesTo ::
  (P.WriterOptions -> P.Pandoc -> P.PandocPure T.Text) ->
  [P.Inline] ->
  T.Text
inlinesTo wr ill =
  case P.runPure . wr P.def $ doc of
    Right t -> T.strip t
    Left e -> error $ "Pandoc write failed: " ++ show e
  where
    doc = P.Pandoc P.nullMeta [P.Plain ill]

stringify :: [P.Inline] -> T.Text
stringify = inlinesTo P.writePlain

-- Maybe convert an Org date of form [YYYY-MM-DD WWW( HH:MM)?] to a date of the
-- form YYYY-MM-DD.
orgDateToIso :: T.Text -> Maybe T.Text
orgDateToIso (T.unpack -> date) =
  case date
    =~ ( "\\[([0-9]+)-([0-9]+)-([0-9]+) [A-Za-z]+( [0-9:]+)?\\]" ::
           String
       ) of
    AllTextSubmatches [_, year, month, day, _time] ->
      Just $ T.pack $ mconcat [year, "-", month, "-", day]
    _ -> Nothing
