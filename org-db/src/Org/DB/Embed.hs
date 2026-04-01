{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Org.DB.Embed (
  EmbedConfig (..),
  EmbedResult (..),
  embedEntries,
  embedQuery,
) where

import Control.Exception (IOException, SomeException, try)
import Control.Monad (when)
import Data.Aeson (FromJSON (..), ToJSON (..), eitherDecode, encode, object, withObject, (.:), (.=))
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
  , embedDimensions :: !(Maybe Int)
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
        when (processed == 0) $ ensureEmbeddingIndex db
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

candidateIdsSQL :: Text
candidateIdsSQL =
  "WITH entry_text AS (\
  \  SELECT e.id,\
  \    COALESCE(e.title, '') || E'\\n' ||\
  \    COALESCE((\
  \      SELECT string_agg(bb.content, E'\\n' ORDER BY bb.position)\
  \      FROM entry_body_blocks bb\
  \      WHERE bb.entry_id = e.id AND bb.content IS NOT NULL\
  \    ), '') || E'\\n' ||\
  \    COALESCE((\
  \      SELECT string_agg(lb.content, E'\\n' ORDER BY le.position, lb.position)\
  \      FROM entry_log_entries le\
  \      JOIN log_entry_body_blocks lb ON lb.log_entry_id = le.id\
  \      WHERE le.entry_id = e.id AND lb.content IS NOT NULL\
  \    ), '') || E'\\n' ||\
  \    COALESCE((\
  \      SELECT string_agg(ep.name || ': ' || ep.value, E'\\n' ORDER BY ep.name)\
  \      FROM entry_properties ep\
  \      WHERE ep.entry_id = e.id\
  \    ), '') AS combined_text\
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

-- | Fetch structured text segments for an entry, each labeled with its source.
fetchEntrySegments :: DBHandle -> Text -> IO [TextSegment]
fetchEntrySegments db entryId = do
  titleSegs <- fetchTitleSegment db entryId
  bodySegs <- fetchBodySegments db entryId
  logSegs <- fetchLogSegments db entryId
  propSegs <- fetchPropertySegment db entryId
  pure (titleSegs ++ bodySegs ++ logSegs ++ propSegs)

fetchTitleSegment :: DBHandle -> Text -> IO [TextSegment]
fetchTitleSegment db entryId = do
  rows <-
    dbQuery
      db
      "SELECT COALESCE(title, '') FROM entries WHERE id = ?"
      [SqlText entryId] ::
      IO [[SqlValue]]
  pure
    [ TextSegment "title" t
    | [SqlText t] <- rows
    , not (T.null (T.strip t))
    ]

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

-- | Build labeled, position-numbered chunks for a single entry.
buildEntryChunks :: DBHandle -> EmbedConfig -> Text -> IO [(Text, Int, Text, Text)]
buildEntryChunks db cfg entryId = do
  segments <- fetchEntrySegments db entryId
  let labeled =
        concatMap
          ( \seg ->
              map (segSource seg,) (chunkText (embedChunkSize cfg) (segText seg))
          )
          segments
  pure (zipWith (\pos (src, txt) -> (entryId, pos, txt, src)) [0 ..] labeled)

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
      IO (Either IOException [EmbeddingData])
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
    "UPDATE entries SET embedding_hash = md5(\
    \  COALESCE(title, '') || E'\\n' ||\
    \  COALESCE((\
    \    SELECT string_agg(bb.content, E'\\n' ORDER BY bb.position)\
    \    FROM entry_body_blocks bb\
    \    WHERE bb.entry_id = entries.id AND bb.content IS NOT NULL\
    \  ), '') || E'\\n' ||\
    \  COALESCE((\
    \    SELECT string_agg(lb.content, E'\\n' ORDER BY le.position, lb.position)\
    \    FROM entry_log_entries le\
    \    JOIN log_entry_body_blocks lb ON lb.log_entry_id = le.id\
    \    WHERE le.entry_id = entries.id AND lb.content IS NOT NULL\
    \  ), '') || E'\\n' ||\
    \  COALESCE((\
    \    SELECT string_agg(ep.name || ': ' || ep.value, E'\\n' ORDER BY ep.name)\
    \    FROM entry_properties ep\
    \    WHERE ep.entry_id = entries.id\
    \  ), '')\
    \) WHERE id = ?"
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
            , _erqDimensions = embedDimensions cfg
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
        ( "Embedding API returned status "
            <> show sc
            <> ": "
            <> take 500 (show (LBS.take 1000 (responseBody resp)))
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
-- Utilities
------------------------------------------------------------------------

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (h, t) = splitAt n xs in h : chunksOf n t

unique :: (Eq a) => [a] -> [a]
unique [] = []
unique (x : xs) = x : unique (filter (/= x) xs)

{- | Create HNSW index on the embedding column if it doesn't already exist.
Best-effort: the untyped vector column only supports indexing after data
is present, so we call this after the first successful batch.
-}
ensureEmbeddingIndex :: DBHandle -> IO ()
ensureEmbeddingIndex db = do
  result <-
    try
      ( dbExecute_
          db
          "CREATE INDEX IF NOT EXISTS idx_entry_embeddings_vector \
          \ON entry_embeddings USING hnsw (embedding vector_cosine_ops) \
          \WHERE embedding IS NOT NULL"
          []
      ) ::
      IO (Either SomeException ())
  case result of
    Right () -> pure ()
    Left _ -> pure () -- Index creation may fail on untyped vector; that's OK
