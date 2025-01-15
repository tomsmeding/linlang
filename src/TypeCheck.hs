{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
module TypeCheck (typeCheck, TypeErr(..), ppTypeErr) where

import Control.Monad
import Data.Foldable (toList)
import Data.List (find, sort, intercalate)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Map (Map)
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.Set (Set)

import AST
import Pretty


type M = Either TypeErr

data TypeErr
  = DuplicateDataDef UName
  | DuplicateDataCon UName
  | OutScope (Either LName UName)
  | Mismatch (Expr ()) Typ Typ  -- problematic expression, expected, actual
  | ConArity UName Int Int  -- constructor name, arity, number of arguments received
  | CaseNonData (Expr ()) Typ  -- expression had this type (which is not a data type)
  | CaseWrongCons UName [UName] [UName]  -- data type, its constructors, constructors listed in case clauses
  | NonlinLamCapturesLin (Expr ()) [LName]
  | LinVarsUsedTwice [LName] (Expr ())
  | DataConInconsistentLin DataDef UName
  | BranchInconsistentLinUsage (Expr ()) (NonEmpty [LName])
  | UnusedLin LName (Expr ())
  deriving (Show)

ppTypeErr :: TypeErr -> String
ppTypeErr = \case
  DuplicateDataDef n -> "Duplicate data definition: " ++ ppUName n
  DuplicateDataCon n -> "Duplicate data constructor: " ++ ppUName n
  OutScope n -> "Out of scope: " ++ either id ppUName n
  Mismatch e expected actual ->
    "Type mismatch:\nexpression: " ++ ppExpr 0 e
    ++ "\nexpected: " ++ ppTyp 0 expected
    ++ "\nactual: " ++ ppTyp 0 actual
  ConArity n arity given ->
    "Constructor " ++ ppUName n
    ++ " takes " ++ show arity ++ " arguments but was given " ++ show given
  CaseNonData e t ->
    "Expression: " ++ ppExpr 0 e
    ++ "\nof type: " ++ ppTyp 0 t
    ++ "\nwas used in a 'case' expression but is not a data type"
  CaseWrongCons dataname defcons cons ->
    "Case on expression of type " ++ ppUName dataname
    ++ "\nwith constructors: " ++ intercalate ", " (map ppUName defcons)
    ++ "\nwas case-matched on giving instead: " ++ intercalate ", " (map ppUName cons)
  NonlinLamCapturesLin lam used ->
    "Non-linear lambda captures linear variables: " ++ intercalate ", " used
    ++ "\nin expression: " ++ ppExpr 0 lam
  LinVarsUsedTwice vars expr ->
    "Linear variables used more than once: " ++ intercalate ", " vars
    ++ "\nin expression: " ++ ppExpr 0 expr
  DataConInconsistentLin def con ->
    "Data constructor " ++ ppUName con ++ "has linearity inconsistent with data type:"
    ++ "\n" ++ ppDataDef def
  BranchInconsistentLinUsage expr uses ->
    "Branches have inconsistent usages of linear variables"
    ++ "\nin expression: " ++ ppExpr 0 expr
    ++ "\nBranches:"
    ++ concat ["\n- " ++ (if null vars then "<none>" else intercalate ", " vars)
              | vars <- toList uses]
  UnusedLin var expr ->
    "Linear variable '" ++ var ++ "' was unused in expression:"
    ++ "\n" ++ ppExpr 0 expr

typeCheck :: Program () -> Either TypeErr (Program Typ)
typeCheck (Program defs main) = do
  (dataenv, conenv) <- checkDataDefs defs
  (main', used) <- let ?conenv = conenv ; ?dataenv = dataenv
                   in checkExpr mempty main
  when (not (Set.null used)) $
    error $ "Non-empty used set of top-level program? " ++ show used
  return (Program defs main')

checkTyp :: Set UName -> Typ -> M ()
checkTyp datas (TData name)
  | name `Set.member` datas = return ()
  | otherwise = Left $ OutScope (Right name)
checkTyp datas (TFun t1 t2) = checkTyp datas t1 >> checkTyp datas t2
checkTyp datas (TLinFun t1 t2) = checkTyp datas t1 >> checkTyp datas t2

type DataEnv = Map UName DataDef
type ConEnv = Map UName ([Typ], Typ)  -- conname => (field types, return type)
type VarEnv = Map LName Typ

-- | Returns only the constructors for this data definition
checkDataDef :: Set UName -> ConEnv -> DataDef -> M ConEnv
checkDataDef datas seenCons def@(DataDef name@(UName datalin _) cons)
  | Just (conname, _) <- find ((`Map.member` seenCons) . fst) cons =
      Left $ DuplicateDataCon conname
  | otherwise = do
      let duplicates = map fst $ filter ((> 1) . snd) $
                         Map.assocs $ Map.fromListWith (+) [(n, 1::Int) | (n, _) <- toList cons]
      forM_ duplicates $ \n -> Left $ DuplicateDataCon n

      
      forM_ [n | (n@(UName lin _), _) <- toList cons, lin /= datalin] $ \n ->
        Left $ DataConInconsistentLin def n

      forM_ (concatMap snd cons) $ \t -> checkTyp datas t
      return (Map.fromList [(n, (fs, TData name)) | (n, fs) <- toList cons])

checkDataDefs :: [DataDef] -> M (DataEnv, ConEnv)
checkDataDefs defs = do
  let datasCount = Map.fromListWith (+) [(name, 1::Int) | DataDef name _ <- defs]
      duplicates = map fst $ filter ((> 1) . snd) (Map.assocs datasCount)
  forM_ duplicates $ \n -> Left $ DuplicateDataDef n

  let datas = Map.keysSet datasCount

  let loop conenv [] = return conenv
      loop conenv (def : defs') = do
        env' <- checkDataDef datas conenv def
        loop (conenv <> env') defs'
  conenv <- loop mempty defs

  return (Map.fromList [(name, def) | def@(DataDef name _) <- defs]
         ,conenv)

-- | Returns typed expression and used linear variables
checkExpr :: (?dataenv :: DataEnv, ?conenv :: ConEnv) => VarEnv -> Expr () -> M (Expr Typ, Set LName)
checkExpr env topexpr = case topexpr of
  EVar () name ->
    case Map.lookup name env of
      Just t -> return (EVar t name, if isLinear t then Set.singleton name else mempty)
      Nothing -> Left $ OutScope (Left name)

  ERecfun funname t1 mt2 var e -> do
    -- Awkward: if no t2 was specified (which means that the source contained a
    -- non-recursive nonlinear lambda), we have no type for the recursor, so we
    -- can't pass it at all in this simplistic type checker. But since the
    -- source was non-recursive anyway, this ought not to be an issue.
    let env' = Map.insert var t1 $ case mt2 of Just t2 -> Map.insert funname (TFun t1 t2) env
                                               Nothing -> env
    (e', used) <- checkExpr env' e

    -- ensure the argument is used correctly
    used' <- scopeVariable var t1 e used

    -- nonlinear lambdas cannot capture linear values
    when (not (Set.null used')) $
      Left $ NonlinLamCapturesLin topexpr (Set.toList used')

    return (ERecfun funname t1 (Just (typeOf e')) var e', used')

  ELinLam var ty e -> do
    (e', used) <- checkExpr (Map.insert var ty env) e

    -- ensure the argument is used correctly
    used' <- scopeVariable var ty e used

    return (ELinLam var ty e', used')

  ELet var e1 e2 -> do
    (e1', used1) <- checkExpr env e1
    let ty = typeOf e1'
    (e2', used2) <- checkExpr (Map.insert var ty env) e2
    used2' <- scopeVariable var ty e2 used2
    reportConflicts [used1, used2']
    return (ELet var e1' e2', used1 <> used2')

  EApp e1 e2 -> do
    (e1', used1) <- checkExpr env e1
    (e2', used2) <- checkExpr env e2
    reportConflicts [used1, used2]
    case (typeOf e1', typeOf e2') of
      (TFun t1 _, t1') | t1 == t1' -> return (EApp e1' e2', used1 <> used2)
                       | otherwise -> Left $ Mismatch e2 t1 t1'
      (TLinFun t1 _, t1') | t1 == t1' -> return (EApp e1' e2', used1 <> used2)
                          | otherwise -> Left $ Mismatch e2 t1 t1'
      (t1, _) -> Left $ Mismatch e1 (TFun (placeh "_") (placeh "_")) t1

  ECon () name args ->
    case Map.lookup name ?conenv of
      Just (fields, retty)
        | length fields == length args -> do
            (args', useds) <- unzip <$> mapM (checkExpr env) args
            reportConflicts useds
            forM_ (zip3 args args' fields) $ \(arg, arg', f) ->
              when (typeOf arg' /= f) $
                Left $ Mismatch arg f (typeOf arg')
            return (ECon retty name args', Set.unions useds)
        | otherwise ->
            Left $ ConArity name (length fields) (length args)
      Nothing ->
        Left $ OutScope (Right name)

  ECase () e1 clauses -> do
    (e1', used1) <- checkExpr env e1
    defname <- case typeOf e1' of
                 TData name -> return name
                 t -> Left $ CaseNonData e1 t
    defcons <- case Map.lookup defname ?dataenv of
                 Just (DataDef _ defcons) -> return defcons
                 Nothing -> error $ "type of expression out of scope? (" ++ ppUName defname ++ ")"

    (let clausenames = sort [n | (n,_,_) <- toList clauses]
         defnames = sort (map fst (toList defcons))
     in when (clausenames /= defnames) $
          Left $ CaseWrongCons defname defnames clausenames)

    (clauses', used2s) <- fmap NE.unzip . forM clauses $ \(conname, vars, rhs) -> do
      let fields = fromJust $ lookup conname (toList defcons)
      when (length fields /= length vars) $ Left $ ConArity conname (length fields) (length vars)
      (rhs', used2) <- checkExpr (foldr (uncurry Map.insert) env (zip vars fields)) rhs
      used2' <- foldM (\u (var, ty) -> scopeVariable var ty rhs u)
                      used2 (zip vars fields)
      return ((conname, vars, rhs'), used2')

    when (not (allEqual used2s)) $
      Left $ BranchInconsistentLinUsage topexpr (fmap Set.toList used2s)
    let used2 = NE.head used2s
    reportConflicts [used1, used2]

    let retty = let (_, _, rhs1) = NE.head clauses' in typeOf rhs1
    forM_ (zip (NE.tail clauses) (NE.tail clauses')) $ \((_, _, rhs), (_, _, rhs')) ->
      when (typeOf rhs' /= retty) $ Left $ Mismatch rhs retty (typeOf rhs')

    return (ECase retty e1' clauses', used1 <> used2)
  where
    reportConflicts :: Foldable t => t (Set LName) -> M ()
    reportConflicts useds =
      let conflicts = Map.filter (> 1) $ Map.fromListWith (+) [(n, 1::Int) | u <- toList useds, n <- Set.toList u]
      in when (not (Map.null conflicts)) $
           Left $ LinVarsUsedTwice (Map.keys conflicts) topexpr

    -- | Given variable x and its type, the expression that is its scope, and
    -- the set of used variables in that expression, compute the set of
    -- variables used _outside_ x's scope.
    scopeVariable :: LName -> Typ -> Expr () -> Set LName -> M (Set LName)
    scopeVariable var ty expr used
      | isLinear ty = if var `Set.member` used
                        then return $ Set.delete var used
                        else Left $ UnusedLin var expr
      | otherwise = return used

    placeh :: String -> Typ
    placeh = TData . UName Nonlin

allEqual :: (Foldable t, Eq a) => t a -> Bool
allEqual l = case toList l of
               [] -> True
               x : xs -> all (== x) xs
