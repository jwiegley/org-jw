{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Org.DB.Embed (
  EmbedConfig (..),
  EmbedResult (..),
  embedEntries,
  embedTitles,
  embedQuery,
) where

import Control.Exception (SomeException, try)

import Data.Aeson (FromJSON (..), ToJSON (..), eitherDecode, encode, object, withObject, (.:), (.=))
import Data.Bifunctor (second)
import Data.ByteString.Lazy qualified as LBS
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import Numeric (showFFloat)
import Org.DB.Types

-- | Configuration for the embedding API (OpenAI-compatible).
data EmbedConfig = EmbedConfig
  { embedBaseUrl :: !Text
  , embedModel :: !Text
  , embedApiKey :: !Text
  , embedBatchSize :: !Int
  , embedDimensions :: !Int
  , embedChunkSize :: !Int
  }

-- | Result of an embedding run.
data EmbedResult = EmbedResult
  { embedProcessed :: !Int
  , embedFailed :: !Int
  , embedErrors :: ![Text]
  }
  deriving (Show)

------------------------------------------------------------------------
-- OpenAI-compatible /v1/embeddings API types
------------------------------------------------------------------------

data EmbeddingRequest = EmbeddingRequest
  { _erqModel :: !Text
  , _erqInput :: ![Text]
  , _erqDimensions :: !(Maybe Int)
  }

instance ToJSON EmbeddingRequest where
  toJSON req =
    object $
      [ "model" .= _erqModel req
      , "input" .= _erqInput req
      ]
        ++ maybe [] (\d -> ["dimensions" .= d]) (_erqDimensions req)

data EmbeddingData = EmbeddingData
  { _edEmbedding :: ![Double]
  , _edIndex :: !Int
  }

instance FromJSON EmbeddingData where
  parseJSON = withObject "EmbeddingData" $ \v ->
    EmbeddingData <$> v .: "embedding" <*> v .: "index"

newtype EmbeddingResponse = EmbeddingResponse
  { _erespData :: [EmbeddingData]
  }

instance FromJSON EmbeddingResponse where
  parseJSON = withObject "EmbeddingResponse" $ \v ->
    EmbeddingResponse <$> v .: "data"

------------------------------------------------------------------------
-- Text chunking
------------------------------------------------------------------------

{- | Split text into chunks by paragraphs (double newlines), merging small
paragraphs together and splitting oversized ones by lines.
-}
chunkText :: Int -> Text -> [Text]
chunkText maxChars txt
  | T.null (T.strip txt) = []
  | T.length txt <= maxChars = [txt]
  | otherwise =
      let paragraphs = filter (not . T.null . T.strip) (T.splitOn "\n\n" txt)
       in mergeParagraphs maxChars [] paragraphs

{- | Merge paragraphs into chunks that fit within maxChars.
Oversized paragraphs are split by lines.
-}
mergeParagraphs :: Int -> [Text] -> [Text] -> [Text]
mergeParagraphs _ acc [] =
  case acc of
    [] -> []
    _ -> [T.intercalate "\n\n" (reverse acc)]
mergeParagraphs maxChars acc (p : ps)
  | T.length p > maxChars =
      -- Flush accumulator, then split this paragraph by lines
      let flushed = case acc of
            [] -> []
            _ -> [T.intercalate "\n\n" (reverse acc)]
          lineChunks = splitByLines maxChars p
       in flushed ++ lineChunks ++ mergeParagraphs maxChars [] ps
  | accLen + sepLen + T.length p > maxChars =
      -- Current accumulator is full, flush it
      T.intercalate "\n\n" (reverse acc)
        : mergeParagraphs maxChars [p] ps
  | otherwise =
      mergeParagraphs maxChars (p : acc) ps
 where
  accLen = sum (map T.length acc) + max 0 (2 * (length acc - 1))
  sepLen = if null acc then 0 else 2

-- | Split a paragraph by lines, merging lines into chunks.
splitByLines :: Int -> Text -> [Text]
splitByLines maxChars para =
  let lns = T.lines para
   in mergeLines maxChars [] lns

mergeLines :: Int -> [Text] -> [Text] -> [Text]
mergeLines _ acc [] =
  case acc of
    [] -> []
    _ -> [T.intercalate "\n" (reverse acc)]
mergeLines maxChars acc (l : ls)
  | T.length l > maxChars =
      -- Single line too long — hard split at maxChars
      let flushed = case acc of
            [] -> []
            _ -> [T.intercalate "\n" (reverse acc)]
          hardChunks = hardSplit maxChars l
       in flushed ++ hardChunks ++ mergeLines maxChars [] ls
  | accLen + sepLen + T.length l > maxChars =
      T.intercalate "\n" (reverse acc) : mergeLines maxChars [l] ls
  | otherwise =
      mergeLines maxChars (l : acc) ls
 where
  accLen = sum (map T.length acc) + max 0 (length acc - 1)
  sepLen = if null acc then 0 else 1

-- | Hard split a text at maxChars boundaries.
hardSplit :: Int -> Text -> [Text]
hardSplit maxChars txt
  | T.null txt = []
  | otherwise =
      let (chunk, rest) = T.splitAt maxChars txt
       in chunk : hardSplit maxChars rest

------------------------------------------------------------------------
-- Main embedding function
------------------------------------------------------------------------

{- | Embed all entries that need embedding (new or changed).
The progress callback receives (chunks processed so far, total chunks).
-}
embedEntries :: DBHandle -> EmbedConfig -> (Int -> Int -> IO ()) -> IO EmbedResult
embedEntries db cfg progress = do
  manager <- newManager tlsManagerSettings
  candidateIds <- fetchCandidateIds db
  if null candidateIds
    then pure (EmbedResult 0 0 [])
    else do
      -- Delete stale chunks for entries that will be re-embedded
      dbTransaction db $
        mapM_
          (\eid -> dbExecute_ db "DELETE FROM entry_embeddings WHERE entry_id = ?" [SqlText eid])
          candidateIds
      -- Build labeled chunks from structured segments
      allChunks <- concatMapM (buildEntryChunks db cfg) candidateIds
      let total = length allChunks
          batches = chunksOf (max 1 (embedBatchSize cfg)) allChunks
      go manager batches 0 [] total
 where
  go _ [] processed errs _ =
    pure (EmbedResult processed (length errs) (reverse errs))
  go manager (batch : rest) processed errs total = do
    result <- processChunkBatch db cfg manager batch
    case result of
      Right n -> do
        let processed' = processed + n
        progress processed' total
        go manager rest processed' errs total
      Left err ->
        go manager rest processed (err : errs) total

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = concat <$> mapM f xs

------------------------------------------------------------------------
-- Fetch entries needing embedding
------------------------------------------------------------------------

-- | Find entry IDs whose content has changed since last embedding.
fetchCandidateIds :: DBHandle -> IO [Text]
fetchCandidateIds db = do
  rows <- dbQuery db candidateIdsSQL [] :: IO [[SqlValue]]
  pure [eid | [SqlText eid] <- rows]

{- | SQL fragment that computes the combined text for hash comparison.
Includes ancestor titles, tags, and categories so that embeddings are
re-generated when an entry's organizational context changes.
-}
combinedTextExpr :: Text
combinedTextExpr =
  "COALESCE((\
  \  WITH RECURSIVE anc AS (\
  \    SELECT parent_id FROM entries WHERE id = e.id \
  \    UNION ALL \
  \    SELECT e2.parent_id FROM entries e2 JOIN anc a ON e2.id = a.parent_id \
  \    WHERE e2.parent_id IS NOT NULL\
  \  ) SELECT string_agg(e2.title, ' > ' ORDER BY e2.depth) \
  \    FROM anc a JOIN entries e2 ON a.parent_id = e2.id\
  \), '') || E'\\n' ||\
  \COALESCE((\
  \  SELECT string_agg(t.tag, ', ' ORDER BY t.tag) \
  \  FROM entry_tags t WHERE t.entry_id = e.id\
  \), '') || E'\\n' ||\
  \COALESCE((\
  \  SELECT string_agg(c.category, ', ' ORDER BY c.category) \
  \  FROM entry_categories c WHERE c.entry_id = e.id\
  \), '') || E'\\n' ||\
  \COALESCE(e.title, '') || E'\\n' ||\
  \COALESCE((\
  \  SELECT string_agg(bb.content, E'\\n' ORDER BY bb.position)\
  \  FROM entry_body_blocks bb\
  \  WHERE bb.entry_id = e.id AND bb.content IS NOT NULL\
  \), '') || E'\\n' ||\
  \COALESCE((\
  \  SELECT string_agg(lb.content, E'\\n' ORDER BY le.position, lb.position)\
  \  FROM entry_log_entries le\
  \  JOIN log_entry_body_blocks lb ON lb.log_entry_id = le.id\
  \  WHERE le.entry_id = e.id AND lb.content IS NOT NULL\
  \), '') || E'\\n' ||\
  \COALESCE((\
  \  SELECT string_agg(ep.name || ': ' || ep.value, E'\\n' ORDER BY ep.name)\
  \  FROM entry_properties ep\
  \  WHERE ep.entry_id = e.id\
  \), '')"

candidateIdsSQL :: Text
candidateIdsSQL =
  "WITH entry_text AS (\
  \  SELECT e.id, "
    <> combinedTextExpr
    <> " AS combined_text\
       \  FROM entries e\
       \) \
       \SELECT et.id \
       \FROM entry_text et \
       \JOIN entries e ON e.id = et.id \
       \WHERE e.embedding_hash IS NULL \
       \   OR e.embedding_hash IS DISTINCT FROM md5(et.combined_text)"

------------------------------------------------------------------------
-- Labeled text segments
------------------------------------------------------------------------

-- | A text segment with its source path within the entry.
data TextSegment = TextSegment
  { segSource :: !Text
  -- ^ e.g. "title", "body[3]", "log[5]/body[2]", "properties"
  , segText :: !Text
  }

{- | Fetch content segments for an entry (body, log, properties).
The context header (path, title, tags, categories) is prepended to every
chunk separately in 'buildEntryChunks'.
-}
fetchContentSegments :: DBHandle -> Text -> IO [TextSegment]
fetchContentSegments db entryId = do
  bodySegs <- fetchBodySegments db entryId
  logSegs <- fetchLogSegments db entryId
  propSegs <- fetchPropertySegment db entryId
  pure (bodySegs ++ logSegs ++ propSegs)

{- | Build a header that is prepended to every embedded chunk for an entry.
Includes the ancestor path, title, tags, and categories so that each chunk
carries enough context for semantic search (e.g. a body chunk under
"Pool > Pool leak repair" will contain the word "pool").
-}
fetchChunkHeader :: DBHandle -> Text -> IO Text
fetchChunkHeader db entryId = do
  ancestors <- fetchAncestorTitles db entryId
  title <- fetchTitleText db entryId
  tags <- fetchEntryTags db entryId
  cats <- fetchEntryCategories db entryId
  let parts =
        ["Path: " <> T.intercalate " > " ancestors | not (null ancestors)]
          ++ ["Title: " <> title | not (T.null title)]
          ++ ["Tags: " <> T.intercalate ", " tags | not (null tags)]
          ++ ["Categories: " <> T.intercalate ", " cats | not (null cats)]
  pure (T.intercalate "\n" parts)

fetchAncestorTitles :: DBHandle -> Text -> IO [Text]
fetchAncestorTitles db entryId = do
  rows <-
    dbQuery
      db
      "WITH RECURSIVE anc AS (\
      \  SELECT parent_id FROM entries WHERE id = ? \
      \  UNION ALL \
      \  SELECT e.parent_id FROM entries e JOIN anc a ON e.id = a.parent_id \
      \  WHERE e.parent_id IS NOT NULL\
      \) \
      \SELECT e.title FROM anc a JOIN entries e ON a.parent_id = e.id"
      [SqlText entryId] ::
      IO [[SqlValue]]
  pure (reverse [t | [SqlText t] <- rows])

fetchEntryTags :: DBHandle -> Text -> IO [Text]
fetchEntryTags db entryId = do
  rows <-
    dbQuery
      db
      "SELECT tag FROM entry_tags WHERE entry_id = ? ORDER BY tag"
      [SqlText entryId] ::
      IO [[SqlValue]]
  pure [t | [SqlText t] <- rows]

fetchEntryCategories :: DBHandle -> Text -> IO [Text]
fetchEntryCategories db entryId = do
  rows <-
    dbQuery
      db
      "SELECT category FROM entry_categories WHERE entry_id = ? ORDER BY category"
      [SqlText entryId] ::
      IO [[SqlValue]]
  pure [t | [SqlText t] <- rows]

fetchTitleText :: DBHandle -> Text -> IO Text
fetchTitleText db entryId = do
  rows <-
    dbQuery
      db
      "SELECT COALESCE(title, '') FROM entries WHERE id = ?"
      [SqlText entryId] ::
      IO [[SqlValue]]
  pure $ case rows of
    ([SqlText t] : _) -> T.strip t
    _ -> ""

fetchBodySegments :: DBHandle -> Text -> IO [TextSegment]
fetchBodySegments db entryId = do
  rows <-
    dbQuery
      db
      "SELECT position, COALESCE(content, '') \
      \FROM entry_body_blocks \
      \WHERE entry_id = ? AND content IS NOT NULL \
      \ORDER BY position"
      [SqlText entryId] ::
      IO [[SqlValue]]
  pure
    [ TextSegment ("body[" <> T.pack (show pos) <> "]") content
    | [SqlInt pos, SqlText content] <- rows
    , not (T.null (T.strip content))
    ]

fetchLogSegments :: DBHandle -> Text -> IO [TextSegment]
fetchLogSegments db entryId = do
  rows <-
    dbQuery
      db
      "SELECT le.position, lb.position, COALESCE(lb.content, '') \
      \FROM entry_log_entries le \
      \JOIN log_entry_body_blocks lb ON lb.log_entry_id = le.id \
      \WHERE le.entry_id = ? AND lb.content IS NOT NULL \
      \ORDER BY le.position, lb.position"
      [SqlText entryId] ::
      IO [[SqlValue]]
  pure
    [ TextSegment
        ("log[" <> T.pack (show lePos) <> "]/body[" <> T.pack (show lbPos) <> "]")
        content
    | [SqlInt lePos, SqlInt lbPos, SqlText content] <- rows
    , not (T.null (T.strip content))
    ]

fetchPropertySegment :: DBHandle -> Text -> IO [TextSegment]
fetchPropertySegment db entryId = do
  rows <-
    dbQuery
      db
      "SELECT name, value FROM entry_properties \
      \WHERE entry_id = ? ORDER BY name"
      [SqlText entryId] ::
      IO [[SqlValue]]
  let propText =
        T.intercalate "\n" [n <> ": " <> v | [SqlText n, SqlText v] <- rows]
  pure
    [ TextSegment "properties" propText
    | not (T.null (T.strip propText))
    ]

------------------------------------------------------------------------
-- Chunk building with source labels
------------------------------------------------------------------------

{- | Build labeled, position-numbered chunks for a single entry.
Every chunk is prefixed with the entry's context header (path, title,
tags, categories) so that each embedded chunk is self-contained for search.
-}
buildEntryChunks :: DBHandle -> EmbedConfig -> Text -> IO [(Text, Int, Text, Text)]
buildEntryChunks db cfg entryId = do
  header <- fetchChunkHeader db entryId
  segments <- fetchContentSegments db entryId
  let headerPrefix = if T.null header then "" else header <> "\n\n"
      contentSize = max 100 (embedChunkSize cfg - T.length headerPrefix)
      labeled =
        concatMap
          ( \seg ->
              map (segSource seg,) (chunkText contentSize (segText seg))
          )
          segments
      -- Prepend header to every chunk; if no content, emit header alone
      prefixed = case labeled of
        [] -> [("header", header) | not (T.null header)]
        _ -> map (second (headerPrefix <>)) labeled
  pure (zipWith (\pos (src, txt) -> (entryId, pos, txt, src)) [0 ..] prefixed)

------------------------------------------------------------------------
-- Batch processing
------------------------------------------------------------------------

processChunkBatch ::
  DBHandle ->
  EmbedConfig ->
  Manager ->
  [(Text, Int, Text, Text)] ->
  IO (Either Text Int)
processChunkBatch db cfg manager batch = do
  let texts = map (\(_, _, t, _) -> t) batch
  result <-
    try (callEmbeddingAPI cfg manager texts) ::
      IO (Either SomeException [EmbeddingData])
  case result of
    Left err -> pure (Left (T.pack (show err)))
    Right eds -> do
      let vecs = map _edEmbedding (sortBy (comparing _edIndex) eds)
      if length vecs /= length batch
        then
          pure
            ( Left
                ( "API returned "
                    <> T.pack (show (length vecs))
                    <> " embeddings for "
                    <> T.pack (show (length batch))
                    <> " inputs"
                )
            )
        else do
          dbTransaction db $ do
            mapM_
              ( \((entryId, chunkPos, chunkTxt, chunkSrc), vec) ->
                  storeChunkEmbedding db entryId chunkPos chunkSrc chunkTxt vec
              )
              (zip batch vecs)
            -- Update embedding_hash for all entries in this batch
            let entryIds = map (\(eid, _, _, _) -> eid) batch
            mapM_ (updateEmbeddingHash db) (unique entryIds)
          pure (Right (length batch))

storeChunkEmbedding :: DBHandle -> Text -> Int -> Text -> Text -> [Double] -> IO ()
storeChunkEmbedding db entryId chunkPos chunkSrc chunkTxt vec = do
  let vecText = "[" <> T.intercalate "," (map showDouble vec) <> "]"
  dbExecute_
    db
    "INSERT INTO entry_embeddings \
    \(entry_id, chunk_position, chunk_source, chunk_text, embedding) \
    \VALUES (?, ?, ?, ?, ?::vector) \
    \ON CONFLICT (entry_id, chunk_position) \
    \DO UPDATE SET chunk_source = EXCLUDED.chunk_source, \
    \  chunk_text = EXCLUDED.chunk_text, \
    \  embedding = EXCLUDED.embedding"
    [ SqlText entryId
    , SqlInt (fromIntegral chunkPos)
    , SqlText chunkSrc
    , SqlText chunkTxt
    , SqlText vecText
    ]

{- | Update the embedding_hash on the entry to mark it as current.
Uses a subquery to compute the hash from the same combined_text as candidateSQL.
-}
updateEmbeddingHash :: DBHandle -> Text -> IO ()
updateEmbeddingHash db entryId =
  dbExecute_
    db
    ( "UPDATE entries SET embedding_hash = (\
      \  SELECT md5("
        <> combinedTextExpr
        <> ")\
           \  FROM entries e WHERE e.id = entries.id\
           \) WHERE id = ?"
    )
    [SqlText entryId]

showDouble :: Double -> Text
showDouble d = T.pack (showFFloat Nothing d "")

------------------------------------------------------------------------
-- HTTP client for OpenAI-compatible embedding API
------------------------------------------------------------------------

callEmbeddingAPI :: EmbedConfig -> Manager -> [Text] -> IO [EmbeddingData]
callEmbeddingAPI cfg manager texts = do
  let url = T.unpack (embedBaseUrl cfg) <> "/v1/embeddings"
      reqBody =
        encode
          EmbeddingRequest
            { _erqModel = embedModel cfg
            , _erqInput = texts
            , _erqDimensions = Nothing
            }
  initReq <- parseRequest url
  let req =
        initReq
          { method = "POST"
          , requestBody = RequestBodyLBS reqBody
          , requestHeaders =
              [ ("Content-Type", "application/json")
              , ("Authorization", "Bearer " <> TE.encodeUtf8 (embedApiKey cfg))
              ]
          }
  resp <- httpLbs req manager
  let sc = statusCode (responseStatus resp)
  if sc >= 200 && sc < 300
    then case eitherDecode (responseBody resp) of
      Right (EmbeddingResponse ds) -> pure ds
      Left err -> fail ("Failed to parse embedding response: " <> err)
    else
      fail
        ( "POST "
            <> url
            <> " returned status "
            <> show sc
            <> ":\n"
            <> take 1000 (T.unpack (TE.decodeUtf8With (\_ _ -> Just '?') (LBS.toStrict (LBS.take 1000 (responseBody resp)))))
        )

------------------------------------------------------------------------
-- Query embedding
------------------------------------------------------------------------

-- | Embed a single query text and return its vector.
embedQuery :: EmbedConfig -> Text -> IO [Double]
embedQuery cfg queryText = do
  manager <- newManager tlsManagerSettings
  eds <- callEmbeddingAPI cfg manager [queryText]
  case eds of
    [ed] -> pure (_edEmbedding ed)
    _ -> fail $ "Expected 1 embedding, got " <> show (length eds)

------------------------------------------------------------------------
-- Title embedding
------------------------------------------------------------------------

{- | Embed titles for entries that have content embeddings but are missing
title embeddings. Titles are embedded as-is (no chunking) and stored
directly in the entries.title_embedding column.
-}
embedTitles :: DBHandle -> EmbedConfig -> (Int -> Int -> IO ()) -> IO EmbedResult
embedTitles db cfg progress = do
  manager <- newManager tlsManagerSettings
  candidates <- fetchTitleCandidates db
  if null candidates
    then pure (EmbedResult 0 0 [])
    else do
      let total = length candidates
          batches = chunksOf (max 1 (embedBatchSize cfg)) candidates
      goTitles manager batches 0 [] total
 where
  goTitles _ [] processed errs _ =
    pure (EmbedResult processed (length errs) (reverse errs))
  goTitles manager (batch : rest) processed errs total = do
    result <- processTitleBatch db cfg manager batch
    case result of
      Right n -> do
        let processed' = processed + n
        progress processed' total
        goTitles manager rest processed' errs total
      Left err ->
        goTitles manager rest processed (err : errs) total

{- | Find entries that are missing title embeddings.
Returns (entry_id, title) pairs for all entries with non-empty titles.
-}
fetchTitleCandidates :: DBHandle -> IO [(Text, Text)]
fetchTitleCandidates db = do
  rows <-
    dbQuery
      db
      "SELECT id, COALESCE(title, '') FROM entries \
      \WHERE title_embedding IS NULL \
      \AND COALESCE(title, '') != ''"
      [] ::
      IO [[SqlValue]]
  pure [(eid, title) | [SqlText eid, SqlText title] <- rows]

processTitleBatch ::
  DBHandle ->
  EmbedConfig ->
  Manager ->
  [(Text, Text)] ->
  IO (Either Text Int)
processTitleBatch db cfg manager batch = do
  let titles = map snd batch
  result <-
    try (callEmbeddingAPI cfg manager titles) ::
      IO (Either SomeException [EmbeddingData])
  case result of
    Left err -> pure (Left (T.pack (show err)))
    Right eds -> do
      let vecs = map _edEmbedding (sortBy (comparing _edIndex) eds)
      if length vecs /= length batch
        then
          pure
            ( Left
                ( "API returned "
                    <> T.pack (show (length vecs))
                    <> " title embeddings for "
                    <> T.pack (show (length batch))
                    <> " inputs"
                )
            )
        else do
          dbTransaction db $
            mapM_
              ( \((entryId, _), vec) ->
                  storeTitleEmbedding db entryId vec
              )
              (zip batch vecs)
          pure (Right (length batch))

storeTitleEmbedding :: DBHandle -> Text -> [Double] -> IO ()
storeTitleEmbedding db entryId vec = do
  let vecText = "[" <> T.intercalate "," (map showDouble vec) <> "]"
  dbExecute_
    db
    "UPDATE entries SET title_embedding = ?::vector WHERE id = ?"
    [SqlText vecText, SqlText entryId]

------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (h, t) = splitAt n xs in h : chunksOf n t

unique :: (Eq a) => [a] -> [a]
unique [] = []
unique (x : xs) = x : unique (filter (/= x) xs)
