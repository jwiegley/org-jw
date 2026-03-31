{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Org.DB.Query (
  -- * Query types
  OrgQuery (..),
  Comparator (..),
  TsKind (..),
  DateFilter (..),
  DateVal (..),
  PropMatch (..),
  LinkFilter (..),

  -- * Parsing
  parseOrgQuery,

  -- * Compilation
  compileOrgQuery,

  -- * Execution
  queryByOrgQl,

  -- * SQL helpers
  entrySelectSQL,
) where

import Data.Char (isDigit, isSpace)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (Day, addDays)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Org.DB.Types

------------------------------------------------------------------------
-- S-expression parser
------------------------------------------------------------------------

data Sexp
  = SAtom Text
  | SString Text
  | SInt Int
  | SList [Sexp]
  deriving (Show, Eq)

parseSexp :: Text -> Either String Sexp
parseSexp input =
  case pSexp (T.stripStart input) of
    Left err -> Left err
    Right (sexp, rest)
      | T.null (T.stripStart rest) -> Right sexp
      | otherwise -> Left $ "unexpected trailing input: " ++ T.unpack rest

maxSexpDepth :: Int
maxSexpDepth = 50

pSexp :: Text -> Either String (Sexp, Text)
pSexp = pSexpD 0

pSexpD :: Int -> Text -> Either String (Sexp, Text)
pSexpD depth t
  | depth > maxSexpDepth = Left "S-expression nesting limit exceeded"
  | T.null t = Left "unexpected end of input"
  | T.head t == '(' = pListD depth t
  | T.head t == '"' = pString t
  | isNegInt t = pInt t
  | isDigit (T.head t) = pInt t
  | otherwise = pAtom t
 where
  isNegInt s =
    T.length s > 1
      && T.head s == '-'
      && isDigit (T.index s 1)

pListD :: Int -> Text -> Either String (Sexp, Text)
pListD depth t = do
  let t' = T.stripStart (T.tail t) -- skip '('
  (elems, rest) <- pListElemsD (depth + 1) t'
  pure (SList elems, rest)

pListElemsD :: Int -> Text -> Either String ([Sexp], Text)
pListElemsD _ t
  | T.null t = Left "unexpected end of input in list"
  | T.head t == ')' = Right ([], T.stripStart (T.tail t))
pListElemsD depth t = do
  (sexp, rest) <- pSexpD depth t
  (more, rest') <- pListElemsD depth (T.stripStart rest)
  pure (sexp : more, rest')

pString :: Text -> Either String (Sexp, Text)
pString t = go (T.tail t) "" -- skip opening '"'
 where
  go s acc
    | T.null s = Left "unterminated string"
    | T.head s == '"' = Right (SString (T.pack (reverse acc)), T.tail s)
    | T.head s == '\\' && T.length s > 1 =
        go (T.drop 2 s) (T.index s 1 : acc)
    | otherwise = go (T.tail s) (T.head s : acc)

pInt :: Text -> Either String (Sexp, Text)
pInt t =
  let (numStr, rest) = T.span (\c -> isDigit c || c == '-') t
   in case reads (T.unpack numStr) of
        [(n, "")] -> Right (SInt n, rest)
        _ -> Left $ "invalid integer: " ++ T.unpack numStr

pAtom :: Text -> Either String (Sexp, Text)
pAtom t =
  let (atom, rest) = T.break (\c -> isSpace c || c == '(' || c == ')' || c == '"') t
   in if T.null atom
        then Left $ "unexpected character: " ++ take 1 (T.unpack t)
        else Right (SAtom atom, rest)

------------------------------------------------------------------------
-- OrgQuery AST
------------------------------------------------------------------------

data OrgQuery
  = QAnd [OrgQuery]
  | QOr [OrgQuery]
  | QNot OrgQuery
  | QWhen OrgQuery [OrgQuery]
  | QUnless OrgQuery [OrgQuery]
  | QTodo [Text]
  | QDone
  | QTags [Text]
  | QTagsLocal [Text]
  | QTagsAll [Text]
  | QTagsInherited [Text]
  | QHeading [Text]
  | QRegexp [Text]
  | QRifle [Text]
  | QProperty Text (Maybe PropMatch)
  | QPriority (Maybe Comparator) [Text]
  | QLevel (Maybe Comparator) Int
  | QCategory [Text]
  | QHabit
  | QTs TsKind DateFilter
  | QScheduled DateFilter
  | QDeadline DateFilter
  | QClosed DateFilter
  | QClocked DateFilter
  | QPlanning DateFilter
  | QPath [Text]
  | QLink LinkFilter
  | QParent OrgQuery
  | QAncestors OrgQuery
  | QChildren (Maybe OrgQuery)
  | QDescendants OrgQuery
  deriving (Show, Eq)

data PropMatch
  = PropEquals Text
  | PropCompare Comparator Text
  deriving (Show, Eq)

data Comparator = Lt | Le | Gt | Ge | CmpEq
  deriving (Show, Eq)

data TsKind = TsAny | TsActive | TsInactive
  deriving (Show, Eq)

data DateFilter = DateFilter
  { dfOn :: Maybe DateVal
  , dfFrom :: Maybe DateVal
  , dfTo :: Maybe DateVal
  }
  deriving (Show, Eq)

emptyDateFilter :: DateFilter
emptyDateFilter = DateFilter Nothing Nothing Nothing

data DateVal
  = DateToday
  | DateOffset Int
  | DateAbsolute Text
  deriving (Show, Eq)

data LinkFilter
  = LinkAny
  | LinkTarget Text
  deriving (Show, Eq)

------------------------------------------------------------------------
-- Sexp → OrgQuery translation
------------------------------------------------------------------------

parseOrgQuery :: Text -> Either String OrgQuery
parseOrgQuery input = do
  sexp <- parseSexp input
  sexpToQuery sexp

sexpToQuery :: Sexp -> Either String OrgQuery
sexpToQuery (SAtom a) = bareAtomToQuery a
sexpToQuery (SString _) = Left "unexpected string at top level"
sexpToQuery (SInt _) = Left "unexpected integer at top level"
sexpToQuery (SList []) = Left "empty query"
sexpToQuery (SList (SAtom name : args)) = case name of
  -- Combinators
  "and" -> QAnd <$> mapM sexpToQuery args
  "or" -> QOr <$> mapM sexpToQuery args
  "not" -> case args of
    [q] -> QNot <$> sexpToQuery q
    _ -> Left "not: expected exactly one argument"
  "when" -> case args of
    (c : qs) -> QWhen <$> sexpToQuery c <*> mapM sexpToQuery qs
    _ -> Left "when: expected condition and body"
  "unless" -> case args of
    (c : qs) -> QUnless <$> sexpToQuery c <*> mapM sexpToQuery qs
    _ -> Left "unless: expected condition and body"
  -- TODO state
  "todo" -> QTodo <$> mapM expectString args
  "done" -> expectNoArgs "done" args >> pure QDone
  -- Tags
  "tags" -> QTags <$> mapM expectString args
  "tags-local" -> QTagsLocal <$> mapM expectString args
  "tags-all" -> QTagsAll <$> mapM expectString args
  "tags-inherited" -> QTagsInherited <$> mapM expectString args
  "itags" -> QTagsInherited <$> mapM expectString args
  -- Text matching
  "heading" -> QHeading <$> mapM expectString args
  "headline" -> QHeading <$> mapM expectString args
  "h" -> QHeading <$> mapM expectString args
  "regexp" -> QRegexp <$> mapM expectString args
  "re" -> QRegexp <$> mapM expectString args
  "rifle" -> QRifle <$> mapM expectString args
  -- Property
  "property" -> parseProperty args
  -- Priority
  "priority" -> parsePriority args
  "p" -> parsePriority args
  -- Level
  "level" -> parseLevel args
  -- Category
  "category" -> QCategory <$> mapM expectString args
  -- Habit
  "habit" -> expectNoArgs "habit" args >> pure QHabit
  -- Timestamps
  "ts" -> QTs TsAny <$> parseDateFilter args
  "ts-active" -> QTs TsActive <$> parseDateFilter args
  "ts-a" -> QTs TsActive <$> parseDateFilter args
  "ts-inactive" -> QTs TsInactive <$> parseDateFilter args
  "ts-i" -> QTs TsInactive <$> parseDateFilter args
  "scheduled" -> QScheduled <$> parseDateFilter args
  "deadline" -> QDeadline <$> parseDateFilter args
  "closed" -> QClosed <$> parseDateFilter args
  "clocked" -> QClocked <$> parseDateFilter args
  "planning" -> QPlanning <$> parseDateFilter args
  -- Path
  "path" -> QPath <$> mapM expectString args
  -- Links
  "link" -> parseLink args
  -- Hierarchy
  "parent" -> case args of
    [q] -> QParent <$> sexpToQuery q
    _ -> Left "parent: expected exactly one argument"
  "ancestors" -> case args of
    [q] -> QAncestors <$> sexpToQuery q
    _ -> Left "ancestors: expected exactly one argument"
  "children" -> case args of
    [] -> pure (QChildren Nothing)
    [q] -> QChildren . Just <$> sexpToQuery q
    _ -> Left "children: expected zero or one argument"
  "descendants" -> case args of
    [q] -> QDescendants <$> sexpToQuery q
    _ -> Left "descendants: expected exactly one argument"
  other -> Left $ "unknown predicate: " ++ T.unpack other
sexpToQuery (SList (s : _)) = Left $ "expected predicate name, got: " ++ show s

bareAtomToQuery :: Text -> Either String OrgQuery
bareAtomToQuery = \case
  "todo" -> Right (QTodo [])
  "done" -> Right QDone
  "tags" -> Right (QTags [])
  "habit" -> Right QHabit
  "scheduled" -> Right (QScheduled emptyDateFilter)
  "deadline" -> Right (QDeadline emptyDateFilter)
  "closed" -> Right (QClosed emptyDateFilter)
  "clocked" -> Right (QClocked emptyDateFilter)
  "planning" -> Right (QPlanning emptyDateFilter)
  "link" -> Right (QLink LinkAny)
  a -> Left $ "unexpected bare atom: " ++ T.unpack a

expectString :: Sexp -> Either String Text
expectString (SString s) = Right s
expectString (SAtom a) = Right a
expectString other = Left $ "expected string, got: " ++ show other

expectNoArgs :: String -> [Sexp] -> Either String ()
expectNoArgs name args
  | null args = Right ()
  | otherwise = Left $ name ++ ": expected no arguments"

parseProperty :: [Sexp] -> Either String OrgQuery
parseProperty [SString pname] = Right (QProperty pname Nothing)
parseProperty [SAtom pname] = Right (QProperty pname Nothing)
parseProperty [SString pname, SString pval] =
  Right (QProperty pname (Just (PropEquals pval)))
parseProperty [SAtom pname, SString pval] =
  Right (QProperty pname (Just (PropEquals pval)))
parseProperty [SString pname, SAtom cmp, SString pval] = do
  c <- parseComparator cmp
  pure (QProperty pname (Just (PropCompare c pval)))
parseProperty [SAtom pname, SAtom cmp, SString pval] = do
  c <- parseComparator cmp
  pure (QProperty pname (Just (PropCompare c pval)))
parseProperty _ = Left "property: expected (property NAME [VALUE]) or (property NAME CMP VALUE)"

parsePriority :: [Sexp] -> Either String OrgQuery
parsePriority [] = Right (QPriority Nothing [])
parsePriority [SAtom cmp, SString val] = do
  c <- parseComparator cmp
  pure (QPriority (Just c) [val])
parsePriority [SAtom cmp, SAtom val]
  | isComparator cmp = do
      c <- parseComparator cmp
      pure (QPriority (Just c) [val])
parsePriority args = QPriority Nothing <$> mapM expectString args

parseLevel :: [Sexp] -> Either String OrgQuery
parseLevel [SInt n] = Right (QLevel Nothing n)
parseLevel [SAtom cmp, SInt n] = do
  c <- parseComparator cmp
  pure (QLevel (Just c) n)
parseLevel _ = Left "level: expected (level N) or (level CMP N)"

parseDateFilter :: [Sexp] -> Either String DateFilter
parseDateFilter args = go args emptyDateFilter
 where
  go [] df = Right df
  go (SAtom ":on" : v : rest) df = do
    dv <- parseDateVal v
    go rest df{dfOn = Just dv}
  go (SAtom ":from" : v : rest) df = do
    dv <- parseDateVal v
    go rest df{dfFrom = Just dv}
  go (SAtom ":to" : v : rest) df = do
    dv <- parseDateVal v
    go rest df{dfTo = Just dv}
  go (SAtom ":before" : v : rest) df = do
    dv <- parseDateVal v
    go rest df{dfTo = Just dv}
  go (s : _) _ = Left $ "unexpected in date filter: " ++ show s

parseDateVal :: Sexp -> Either String DateVal
parseDateVal (SAtom "today") = Right DateToday
parseDateVal (SInt n) = Right (DateOffset n)
parseDateVal (SString s)
  | isValidDate s = Right (DateAbsolute s)
  | otherwise = Left $ "invalid date string (expected YYYY-MM-DD): " ++ T.unpack s
parseDateVal other = Left $ "invalid date value: " ++ show other

isValidDate :: Text -> Bool
isValidDate t =
  T.length t == 10
    && T.index t 4 == '-'
    && T.index t 7 == '-'
    && T.all (\c -> isDigit c || c == '-') t

parseLink :: [Sexp] -> Either String OrgQuery
parseLink [] = Right (QLink LinkAny)
parseLink [SString target] = Right (QLink (LinkTarget target))
parseLink [SAtom target] = Right (QLink (LinkTarget target))
parseLink _ = Left "link: expected zero or one argument"

parseComparator :: Text -> Either String Comparator
parseComparator "<" = Right Lt
parseComparator "<=" = Right Le
parseComparator ">" = Right Gt
parseComparator ">=" = Right Ge
parseComparator "=" = Right CmpEq
parseComparator c = Left $ "unknown comparator: " ++ T.unpack c

isComparator :: Text -> Bool
isComparator c = c `elem` ["<", "<=", ">", ">=", "="]

------------------------------------------------------------------------
-- SQL compilation
------------------------------------------------------------------------

type SqlFragment = (Text, [SqlValue])

data CompileCtx = CompileCtx
  { ccAlias :: Text
  , ccDay :: Day
  , ccDepth :: Int
  }

entrySelectSQL :: Text
entrySelectSQL =
  "SELECT e.entry_id, e.file_id, e.parent_id, e.depth, e.keyword, \
  \e.keyword_closed, e.priority, e.headline, e.verb, e.title, \
  \e.context, e.locator, e.file_line, e.path \
  \FROM entries e"

compileOrgQuery :: Day -> OrgQuery -> Either String SqlFragment
compileOrgQuery day = compile (CompileCtx "e" day 0)

maxQueryDepth :: Int
maxQueryDepth = 20

compile :: CompileCtx -> OrgQuery -> Either String SqlFragment
compile ctx query
  | ccDepth ctx > maxQueryDepth =
      Left $ "query nesting limit exceeded (max " ++ show maxQueryDepth ++ ")"
  | otherwise = case query of
      QAnd [] -> Right ("1=1", [])
      QAnd [q] -> compile ctx q
      QAnd qs -> do
        frags <- mapM (compile ctx) qs
        pure (joinFrags " AND " frags)
      QOr [] -> Right ("1=0", [])
      QOr [q] -> compile ctx q
      QOr qs -> do
        frags <- mapM (compile ctx) qs
        pure (joinFrags " OR " frags)
      QNot q -> do
        (sql, ps) <- compile ctx q
        pure ("NOT (" <> sql <> ")", ps)
      QWhen cond qs -> do
        -- (when C Q...) = (NOT C) OR (C AND Q...)
        (csql, cps) <- compile ctx cond
        frags <- mapM (compile ctx) qs
        let (qsql, qps) = joinFrags " AND " frags
        pure
          ( "(NOT (" <> csql <> ") OR (" <> csql <> " AND " <> qsql <> "))"
          , cps <> cps <> qps
          )
      QUnless cond qs -> do
        -- (unless C Q...) = C OR (NOT C AND Q...)
        (csql, cps) <- compile ctx cond
        frags <- mapM (compile ctx) qs
        let (qsql, qps) = joinFrags " AND " frags
        pure
          ( "((" <> csql <> ") OR (NOT (" <> csql <> ") AND " <> qsql <> "))"
          , cps <> cps <> qps
          )
      QTodo [] ->
        pure (a <> ".keyword IS NOT NULL", [])
      QTodo [kw] ->
        pure (a <> ".keyword = ?", [SqlText kw])
      QTodo kws ->
        pure (a <> ".keyword IN (" <> placeholders kws <> ")", map SqlText kws)
      QDone ->
        pure (a <> ".keyword_closed = 1", [])
      QTags [] ->
        pure (existsSubquery "entry_tags" "entry_id" a "1=1" [])
      QTags [tag] ->
        compileTagMatch ctx tag
      QTags tags ->
        do
          frags <- mapM (compileTagMatch ctx) tags
          pure (joinFrags " OR " frags)
      QTagsLocal [] ->
        pure (existsSubquery "entry_tags" "entry_id" a "1=1" [])
      QTagsLocal [tag] ->
        pure (existsTagLocal a tag)
      QTagsLocal tags ->
        let frags = map (existsTagLocal a) tags
         in pure (joinFrags " OR " frags)
      QTagsAll tags -> do
        frags <- mapM (compileTagMatch ctx) tags
        pure (joinFrags " AND " frags)
      QTagsInherited [] ->
        pure (ancestorTagExists ctx Nothing)
      QTagsInherited [tag] ->
        pure (ancestorTagExists ctx (Just tag))
      QTagsInherited tags ->
        let frags = map (ancestorTagExists ctx . Just) tags
         in pure (joinFrags " OR " frags)
      QHeading [] ->
        pure ("1=1", [])
      QHeading [pat] ->
        pure (a <> ".headline LIKE ? ESCAPE '\\'", [SqlText (likePat pat)])
      QHeading pats ->
        pure
          ( "(" <> T.intercalate " OR " (replicate (length pats) (a <> ".headline LIKE ? ESCAPE '\\'")) <> ")"
          , map (SqlText . likePat) pats
          )
      QRegexp pats -> do
        frags <- mapM (compileRegexp ctx) pats
        pure (joinFrags " AND " frags)
      QRifle pats -> do
        frags <- mapM (compileRifle ctx) pats
        pure (joinFrags " AND " frags)
      QProperty pname Nothing ->
        pure (existsSubquery "entry_properties" "entry_id" a "name = ? COLLATE NOCASE" [SqlText pname])
      QProperty pname (Just (PropEquals pval)) ->
        pure (existsSubquery "entry_properties" "entry_id" a "name = ? COLLATE NOCASE AND value = ?" [SqlText pname, SqlText pval])
      QProperty pname (Just (PropCompare cmp pval)) ->
        pure (existsSubquery "entry_properties" "entry_id" a ("name = ? COLLATE NOCASE AND value " <> cmpOp cmp <> " ?") [SqlText pname, SqlText pval])
      QPriority Nothing [] ->
        pure (a <> ".priority IS NOT NULL", [])
      QPriority Nothing [pri] ->
        pure (a <> ".priority = ?", [SqlText pri])
      QPriority Nothing pris ->
        pure (a <> ".priority IN (" <> placeholders pris <> ")", map SqlText pris)
      QPriority (Just cmp) [pri] ->
        pure (a <> ".priority IS NOT NULL AND " <> a <> ".priority " <> cmpOp cmp <> " ?", [SqlText pri])
      QPriority (Just _) _ ->
        Left "priority: comparator requires exactly one value"
      QLevel Nothing n ->
        pure (a <> ".depth = ?", [SqlInt (fromIntegral n)])
      QLevel (Just cmp) n ->
        pure (a <> ".depth " <> cmpOp cmp <> " ?", [SqlInt (fromIntegral n)])
      QCategory [] ->
        pure ("1=1", [])
      QCategory [cat] ->
        pure
          ( "EXISTS (SELECT 1 FROM categories c__ WHERE c__.file_id = " <> a <> ".file_id AND c__.category = ?)"
          , [SqlText cat]
          )
      QCategory cats ->
        pure
          ( "EXISTS (SELECT 1 FROM categories c__ WHERE c__.file_id = " <> a <> ".file_id AND c__.category IN (" <> placeholders cats <> "))"
          , map SqlText cats
          )
      QHabit ->
        pure (existsSubquery "entry_properties" "entry_id" a "name = 'STYLE' AND value = 'habit'" [])
      QTs kind df -> compileTimestamp ctx kind df
      QScheduled df -> compileStampFilter ctx "scheduled" df
      QDeadline df -> compileStampFilter ctx "deadline" df
      QClosed df -> compileStampFilter ctx "closed" df
      QClocked df -> compileClockedFilter ctx df
      QPlanning df -> compilePlanningFilter ctx df
      QPath [] ->
        pure ("1=1", [])
      QPath [pat] ->
        pure (a <> ".path LIKE ? ESCAPE '\\'", [SqlText (likePat pat)])
      QPath pats ->
        pure
          ( "(" <> T.intercalate " OR " (replicate (length pats) (a <> ".path LIKE ? ESCAPE '\\'")) <> ")"
          , map (SqlText . likePat) pats
          )
      QLink LinkAny ->
        pure (existsSubquery "links" "entry_id" a "1=1" [])
      QLink (LinkTarget target) ->
        pure (existsSubquery "links" "entry_id" a "target LIKE ? ESCAPE '\\'" [SqlText (likePat target)])
      QParent q -> compileParent ctx q
      QAncestors q -> compileAncestors ctx q
      QChildren mq -> compileChildren ctx mq
      QDescendants q -> compileDescendants ctx q
 where
  a = ccAlias ctx

-- Tags: (tags "x") matches local OR inherited
compileTagMatch :: CompileCtx -> Text -> Either String SqlFragment
compileTagMatch ctx tag =
  let (localSql, localPs) = existsTagLocal (ccAlias ctx) tag
      (inhSql, inhPs) = ancestorTagExists ctx (Just tag)
   in Right ("(" <> localSql <> " OR " <> inhSql <> ")", localPs <> inhPs)

existsTagLocal :: Text -> Text -> SqlFragment
existsTagLocal a tag =
  ( "EXISTS (SELECT 1 FROM entry_tags et__ WHERE et__.entry_id = "
      <> a
      <> ".entry_id AND et__.tag = ?)"
  , [SqlText tag]
  )

ancestorTagExists :: CompileCtx -> Maybe Text -> SqlFragment
ancestorTagExists ctx mtag =
  let a = ccAlias ctx
      d = T.pack (show (ccDepth ctx))
      cte = "anc" <> d
      tagCond = case mtag of
        Just _ -> " AND et__.tag = ?"
        Nothing -> ""
      params = case mtag of
        Just t -> [SqlText t]
        Nothing -> []
   in ( "EXISTS (WITH RECURSIVE "
          <> cte
          <> "(aid) AS ("
          <> "SELECT "
          <> a
          <> ".parent_id"
          <> " UNION ALL "
          <> "SELECT p__.parent_id FROM entries p__ JOIN "
          <> cte
          <> " ON p__.entry_id = "
          <> cte
          <> ".aid WHERE p__.parent_id IS NOT NULL"
          <> ") SELECT 1 FROM entry_tags et__ JOIN "
          <> cte
          <> " ON et__.entry_id = "
          <> cte
          <> ".aid WHERE "
          <> cte
          <> ".aid IS NOT NULL"
          <> tagCond
          <> ")"
      , params
      )

compileRegexp :: CompileCtx -> Text -> Either String SqlFragment
compileRegexp ctx pat =
  let a = ccAlias ctx
      p = SqlText (likePat pat)
   in Right
        ( "("
            <> a
            <> ".headline LIKE ? ESCAPE '\\' OR EXISTS (SELECT 1 FROM body_blocks bb__ WHERE bb__.entry_id = "
            <> a
            <> ".entry_id AND bb__.content LIKE ? ESCAPE '\\'))"
        , [p, p]
        )

compileRifle :: CompileCtx -> Text -> Either String SqlFragment
compileRifle = compileRegexp

compileStampFilter :: CompileCtx -> Text -> DateFilter -> Either String SqlFragment
compileStampFilter ctx stampType df =
  let a = ccAlias ctx
      (dateConds, dateParams) = compileDateConds (ccDay ctx) "es__.time_start" df
      typeCond = "es__.stamp_type = ?"
      allConds = typeCond : dateConds
   in Right
        ( "EXISTS (SELECT 1 FROM entry_stamps es__ WHERE es__.entry_id = "
            <> a
            <> ".entry_id AND "
            <> T.intercalate " AND " allConds
            <> ")"
        , SqlText stampType : dateParams
        )

compileTimestamp :: CompileCtx -> TsKind -> DateFilter -> Either String SqlFragment
compileTimestamp ctx kind df =
  let a = ccAlias ctx
      (dateConds, dateParams) = compileDateConds (ccDay ctx) "es__.time_start" df
      kindCond = case kind of
        TsAny -> []
        TsActive -> ["es__.time_kind = 'active'"]
        TsInactive -> ["es__.time_kind = 'inactive'"]
      allConds = case kindCond ++ dateConds of
        [] -> ["1=1"]
        cs -> cs
   in Right
        ( "EXISTS (SELECT 1 FROM entry_stamps es__ WHERE es__.entry_id = "
            <> a
            <> ".entry_id AND "
            <> T.intercalate " AND " allConds
            <> ")"
        , dateParams
        )

compileClockedFilter :: CompileCtx -> DateFilter -> Either String SqlFragment
compileClockedFilter ctx df =
  let a = ccAlias ctx
      (dateConds, dateParams) = compileDateConds (ccDay ctx) "le__.log_time" df
      allConds = "le__.log_type = 'clock'" : dateConds
   in Right
        ( "EXISTS (SELECT 1 FROM log_entries le__ WHERE le__.entry_id = "
            <> a
            <> ".entry_id AND "
            <> T.intercalate " AND " allConds
            <> ")"
        , dateParams
        )

compilePlanningFilter :: CompileCtx -> DateFilter -> Either String SqlFragment
compilePlanningFilter ctx df =
  let a = ccAlias ctx
      (dateConds, dateParams) = compileDateConds (ccDay ctx) "es__.time_start" df
      typeCond = "es__.stamp_type IN ('scheduled', 'deadline', 'closed')"
      allConds = typeCond : dateConds
   in Right
        ( "EXISTS (SELECT 1 FROM entry_stamps es__ WHERE es__.entry_id = "
            <> a
            <> ".entry_id AND "
            <> T.intercalate " AND " allConds
            <> ")"
        , dateParams
        )

compileDateConds :: Day -> Text -> DateFilter -> ([Text], [SqlValue])
compileDateConds today col df =
  let onC = case dfOn df of
        Nothing -> ([], [])
        Just dv ->
          let d = resolveDate today dv
           in (["substr(" <> col <> ", 1, 10) = ?"], [SqlText d])
      fromC = case dfFrom df of
        Nothing -> ([], [])
        Just dv ->
          let d = resolveDate today dv
           in (["substr(" <> col <> ", 1, 10) >= ?"], [SqlText d])
      toC = case dfTo df of
        Nothing -> ([], [])
        Just dv ->
          let d = resolveDate today dv
           in (["substr(" <> col <> ", 1, 10) <= ?"], [SqlText d])
      (cs, ps) = unzip [onC, fromC, toC]
   in (concat cs, concat ps)

resolveDate :: Day -> DateVal -> Text
resolveDate today = \case
  DateToday -> formatDay today
  DateOffset n -> formatDay (addDays (fromIntegral n) today)
  DateAbsolute s -> s

formatDay :: Day -> Text
formatDay = T.pack . formatTime defaultTimeLocale "%Y-%m-%d"

compileParent :: CompileCtx -> OrgQuery -> Either String SqlFragment
compileParent ctx q = do
  let d = ccDepth ctx
      pa = "pe" <> T.pack (show d)
      innerCtx = CompileCtx pa (ccDay ctx) (d + 1)
  (innerSql, innerPs) <- compile innerCtx q
  pure
    ( "EXISTS (SELECT 1 FROM entries "
        <> pa
        <> " WHERE "
        <> pa
        <> ".entry_id = "
        <> ccAlias ctx
        <> ".parent_id AND "
        <> innerSql
        <> ")"
    , innerPs
    )

compileAncestors :: CompileCtx -> OrgQuery -> Either String SqlFragment
compileAncestors ctx q = do
  let a = ccAlias ctx
      d = ccDepth ctx
      cte = "ancestors" <> T.pack (show d)
      aa = "ae" <> T.pack (show d)
      innerCtx = CompileCtx aa (ccDay ctx) (d + 1)
  (innerSql, innerPs) <- compile innerCtx q
  pure
    ( "EXISTS (WITH RECURSIVE "
        <> cte
        <> "(aid) AS ("
        <> "SELECT "
        <> a
        <> ".parent_id"
        <> " UNION ALL "
        <> "SELECT p__.parent_id FROM entries p__ JOIN "
        <> cte
        <> " ON p__.entry_id = "
        <> cte
        <> ".aid WHERE p__.parent_id IS NOT NULL"
        <> ") SELECT 1 FROM entries "
        <> aa
        <> " WHERE "
        <> aa
        <> ".entry_id IN (SELECT aid FROM "
        <> cte
        <> " WHERE aid IS NOT NULL) AND "
        <> innerSql
        <> ")"
    , innerPs
    )

compileChildren :: CompileCtx -> Maybe OrgQuery -> Either String SqlFragment
compileChildren ctx Nothing =
  Right
    ( "EXISTS (SELECT 1 FROM entries c__ WHERE c__.parent_id = "
        <> ccAlias ctx
        <> ".entry_id)"
    , []
    )
compileChildren ctx (Just q) = do
  let d = ccDepth ctx
      ca = "ce" <> T.pack (show d)
      innerCtx = CompileCtx ca (ccDay ctx) (d + 1)
  (innerSql, innerPs) <- compile innerCtx q
  pure
    ( "EXISTS (SELECT 1 FROM entries "
        <> ca
        <> " WHERE "
        <> ca
        <> ".parent_id = "
        <> ccAlias ctx
        <> ".entry_id AND "
        <> innerSql
        <> ")"
    , innerPs
    )

compileDescendants :: CompileCtx -> OrgQuery -> Either String SqlFragment
compileDescendants ctx q = do
  let a = ccAlias ctx
      d = ccDepth ctx
      cte = "desc" <> T.pack (show d)
      da = "de" <> T.pack (show d)
      innerCtx = CompileCtx da (ccDay ctx) (d + 1)
  (innerSql, innerPs) <- compile innerCtx q
  pure
    ( "EXISTS (WITH RECURSIVE "
        <> cte
        <> "(did) AS ("
        <> "SELECT entry_id FROM entries WHERE parent_id = "
        <> a
        <> ".entry_id"
        <> " UNION ALL "
        <> "SELECT c__.entry_id FROM entries c__ JOIN "
        <> cte
        <> " ON c__.parent_id = "
        <> cte
        <> ".did"
        <> ") SELECT 1 FROM entries "
        <> da
        <> " WHERE "
        <> da
        <> ".entry_id IN (SELECT did FROM "
        <> cte
        <> ") AND "
        <> innerSql
        <> ")"
    , innerPs
    )

------------------------------------------------------------------------
-- SQL helpers
------------------------------------------------------------------------

existsSubquery :: Text -> Text -> Text -> Text -> [SqlValue] -> SqlFragment
existsSubquery table joinCol alias cond ps =
  ( "EXISTS (SELECT 1 FROM "
      <> table
      <> " t__ WHERE t__."
      <> joinCol
      <> " = "
      <> alias
      <> "."
      <> joinCol
      <> " AND "
      <> cond
      <> ")"
  , ps
  )

placeholders :: [a] -> Text
placeholders xs = T.intercalate ", " (replicate (length xs) "?")

cmpOp :: Comparator -> Text
cmpOp Lt = "<"
cmpOp Le = "<="
cmpOp Gt = ">"
cmpOp Ge = ">="
cmpOp CmpEq = "="

joinFrags :: Text -> [SqlFragment] -> SqlFragment
joinFrags sep frags =
  let sqls = map (\(s, _) -> "(" <> s <> ")") frags
      params = concatMap snd frags
   in (T.intercalate sep sqls, params)

{- | Convert a user pattern to a SQL LIKE pattern (substring match).
Escapes LIKE special characters.
-}
likePat :: Text -> Text
likePat pat = "%" <> escapeLike pat <> "%"

escapeLike :: Text -> Text
escapeLike = T.concatMap $ \case
  '%' -> "\\%"
  '_' -> "\\_"
  '\\' -> "\\\\"
  c -> T.singleton c

------------------------------------------------------------------------
-- Public API
------------------------------------------------------------------------

-- | Execute an org-ql query and return matching entries.
queryByOrgQl :: DBHandle -> Day -> Text -> IO [EntryRow]
queryByOrgQl db today queryStr = do
  query <- either fail pure (parseOrgQuery queryStr)
  (whereClause, params) <- either fail pure (compileOrgQuery today query)
  let sql = entrySelectSQL <> " WHERE " <> whereClause
  dbQuery db sql params
