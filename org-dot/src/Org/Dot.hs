{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Org.Dot (
  -- * DOT generation
  relationshipsDot,
  relationshipsDotText,

  -- * Filtering
  DotConfig (..),
  defaultDotConfig,
) where

import Data.GraphViz
import Data.GraphViz.Attributes.Complete
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Org.DB.Types

-- | Configuration for DOT graph generation.
data DotConfig = DotConfig
  { dotRelationshipTypes :: Maybe [Text]
  -- ^ Filter to these relationship types (Nothing = all)
  , dotFilterFiles :: Maybe [Text]
  -- ^ Only include entries from these files
  , dotFilterTags :: Maybe [Text]
  -- ^ Only include entries with these tags
  , dotFilterKeywords :: Maybe [Text]
  -- ^ Only include entries with these keywords
  , dotMaxDepth :: Maybe Int
  -- ^ Maximum parent-child depth to include
  , dotClusterByFile :: Bool
  -- ^ Group nodes by their file_id into subgraphs
  }
  deriving (Show, Eq)

-- | Default configuration: show all relationships, no filtering, cluster by file.
defaultDotConfig :: DotConfig
defaultDotConfig =
  DotConfig
    { dotRelationshipTypes = Nothing
    , dotFilterFiles = Nothing
    , dotFilterTags = Nothing
    , dotFilterKeywords = Nothing
    , dotMaxDepth = Nothing
    , dotClusterByFile = True
    }

-- | Generate a DOT graph from relationship rows and entry metadata.
relationshipsDot ::
  DotConfig ->
  [RelationshipRow] ->
  [EntryRow] ->
  DotGraph Text
relationshipsDot cfg rels entries =
  let
    -- Build entry lookup
    entryMap = M.fromList [(erId er, er) | er <- entries]
    -- Filter relationships by type
    filteredRels = case dotRelationshipTypes cfg of
      Nothing -> rels
      Just types -> filter (\r -> rrRelationshipType r `elem` types) rels
    -- Collect node IDs from filtered relationships
    nodeIds =
      S.fromList $
        concatMap (\r -> [rrSourceEntryId r, rrTargetEntryId r]) filteredRels
    -- Filter entries to only those in relationships
    nodeEntries = filter (\er -> erId er `S.member` nodeIds) entries
    -- Apply additional filters
    filtered = applyFilters cfg nodeEntries
    filteredIds = S.fromList (map erId filtered)
    -- Final filtered relationships
    finalRels =
      filter
        (\r -> rrSourceEntryId r `S.member` filteredIds && rrTargetEntryId r `S.member` filteredIds)
        filteredRels
    -- Build nodes and edges
    nodes = map (entryToNode entryMap) (S.toList filteredIds)
    edges = map relToEdge finalRels
   in
    if dotClusterByFile cfg
      then buildClusteredGraph cfg entryMap filteredIds finalRels
      else buildFlatGraph nodes edges

-- | Render the DOT graph to Text.
relationshipsDotText ::
  DotConfig ->
  [RelationshipRow] ->
  [EntryRow] ->
  TL.Text
relationshipsDotText cfg rels entries =
  printDotGraph (relationshipsDot cfg rels entries)

------------------------------------------------------------------------
-- Internal
------------------------------------------------------------------------

applyFilters :: DotConfig -> [EntryRow] -> [EntryRow]
applyFilters cfg = applyKeywordFilter . applyFileFilter
 where
  applyFileFilter = case dotFilterFiles cfg of
    Nothing -> id
    Just files -> filter (\er -> erFileId er `elem` files)
  applyKeywordFilter = case dotFilterKeywords cfg of
    Nothing -> id
    Just kws -> filter (\er -> erKeywordValue er `elem` map Just kws)

entryToNode :: M.Map Text EntryRow -> Text -> DotNode Text
entryToNode entryMap eid =
  let label = maybe eid erTitle (M.lookup eid entryMap)
   in DotNode eid [textLabel (TL.fromStrict label)]

relToEdge :: RelationshipRow -> DotEdge Text
relToEdge r =
  DotEdge
    (rrSourceEntryId r)
    (rrTargetEntryId r)
    (edgeAttrs (rrRelationshipType r))

-- | Color-code edges by relationship type.
edgeAttrs :: Text -> Attributes
edgeAttrs "link" = [color Blue, textLabel "link"]
edgeAttrs "blocks" = [color Red, textLabel "blocks", style dashed]
edgeAttrs "blocked_by" = [color Red, textLabel "blocked_by", style dashed]
edgeAttrs "related" = [color ForestGreen, textLabel "related"]
edgeAttrs "parent_child" = [color Gray, style dotted]
edgeAttrs t = [textLabel (TL.fromStrict t)]

buildFlatGraph :: [DotNode Text] -> [DotEdge Text] -> DotGraph Text
buildFlatGraph nodes edges =
  DotGraph
    { strictGraph = False
    , directedGraph = True
    , graphID = Just (Str "relationships")
    , graphStatements =
        DotStmts
          { attrStmts = [GraphAttrs [RankDir FromLeft]]
          , subGraphs = []
          , nodeStmts = nodes
          , edgeStmts = edges
          }
    }

buildClusteredGraph ::
  DotConfig ->
  M.Map Text EntryRow ->
  S.Set Text ->
  [RelationshipRow] ->
  DotGraph Text
buildClusteredGraph _cfg entryMap nodeIds rels =
  let
    -- Group nodes by file_id
    fileGroups =
      M.fromListWith
        (++)
        [ (erFileId er, [eid])
        | eid <- S.toList nodeIds
        , Just er <- [M.lookup eid entryMap]
        ]
    -- Build subgraphs
    subgraphs =
      zipWith
        (buildCluster entryMap)
        [0 ..]
        (M.toList fileGroups)
    edges = map relToEdge rels
   in
    DotGraph
      { strictGraph = False
      , directedGraph = True
      , graphID = Just (Str "relationships")
      , graphStatements =
          DotStmts
            { attrStmts = [GraphAttrs [RankDir FromLeft]]
            , subGraphs = subgraphs
            , nodeStmts = []
            , edgeStmts = edges
            }
      }

buildCluster ::
  M.Map Text EntryRow ->
  Int ->
  (Text, [Text]) ->
  DotSubGraph Text
buildCluster entryMap idx (fileId, nodeIds) =
  DotSG
    { isCluster = True
    , subGraphID = Just (Num (Int idx))
    , subGraphStmts =
        DotStmts
          { attrStmts = [GraphAttrs [textLabel (TL.fromStrict fileId)]]
          , subGraphs = []
          , nodeStmts = map (entryToNode entryMap) nodeIds
          , edgeStmts = []
          }
    }
