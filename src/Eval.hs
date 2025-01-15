{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
module Eval where

import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import Data.Map (Map)

import AST
import Pretty


data Value
  = VData UName [Value]  -- constructor name, fields
  | VFun (Value -> Value)
  | VLinFun (Value -> Value)

ppValue :: Int -> Value -> ShowS
ppValue d = \case
  VData n [] -> showString (ppUName n)
  VData n vs -> showParen (d > 0) $
    showString (ppUName n) . foldr (.) id [showString " " . ppValue 1 v | v <- vs]
  VFun{} -> showString "<function>"
  VLinFun{} -> showString "<linear function>"

evalProgram :: Program Typ -> Value
evalProgram (Program datas main) =
  let ?datas = Map.fromList [(name, def) | def@(DataDef name _) <- datas]
  in evalExpr mempty main

-- | This evaluator blindly assumes that linearity has been checked.
evalExpr :: (?datas :: Map UName DataDef) => Map LName Value -> Expr Typ -> Value
evalExpr env = \case
  EVar _ n -> case Map.lookup n env of
                Just v -> v
                Nothing -> error $ "Out of scope: " ++ n

  ERecfun f _ _ n e ->
    let res = VFun (\v -> evalExpr (Map.insert n v (Map.insert f res env)) e)
    in res

  ELinLam n _ e -> VLinFun (\v -> evalExpr (Map.insert n v env) e)

  ELet n e1 e2 -> evalExpr (Map.insert n (evalExpr env e1) env) e2

  EApp e1 e2 -> case evalExpr env e1 of
                  VFun f -> f (evalExpr env e2)
                  VLinFun f -> f (evalExpr env e2)
                  _ -> error "Cannot call non-function"

  ECon _ n es -> VData n (map (evalExpr env) es)

  ECase _ e clauses ->
    case evalExpr env e of
      VData conname vals
        | [(vars, rhs)] <- [(vars, rhs) | (n, vars, rhs) <- toList clauses, n == conname] ->
            evalExpr (foldr (uncurry Map.insert) env (zip vars vals)) rhs
        | otherwise ->
            error $ "Missing or redundant case for constructor " ++ ppUName conname ++ ""
      VFun{} -> error "Case on function value"
      VLinFun{} -> error "Case on function value"
