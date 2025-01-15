{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Desugar where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict
import Data.List.NonEmpty (NonEmpty(..))

import AST
import Pretty


type M = StateT Int (Either String)

desugarProgram :: Program0 -> Either String (Program ())
desugarProgram (Program0 defs ex) = Program defs <$> evalStateT (desugarExpr ex) 1

desugarExpr :: Expr0 -> M (Expr ())
desugarExpr = \case
  E0Var n -> return $ EVar () n
  E0App e1 e2 -> EApp <$> desugarExpr e1 <*> desugarExpr e2
  E0Con n es -> ECon () n <$> mapM desugarExpr es
  E0Case e1 clauses -> ECase () <$> desugarExpr e1 <*> mapM (\(n, vs, rhs) -> (n,vs,) <$> desugarExpr rhs) clauses

  E0Recfun fun funty arg e ->
    case funty of
      TFun t1 t2 -> ERecfun fun t1 (Just t2) arg <$> desugarExpr e
      TLinFun{}  -> lift $ Left $ "recfun must have nonlinear function type, but is " ++ ppTyp 0 funty ++ ""
      TData{}    -> lift $ Left $ "recfun must have nonlinear function type, not data (" ++ ppTyp 0 funty ++ ")"

  E0Lam Lin n t e -> ELinLam n t <$> desugarExpr e

  E0Lam Nonlin n t1 e -> do
    name <- genName
    ERecfun name t1 Nothing n <$> desugarExpr e

  E0Let PWild e1 e2 -> do
    name <- genName
    ELet name <$> desugarExpr e1 <*> desugarExpr e2
  E0Let (PVar name) e1 e2 -> ELet name <$> desugarExpr e1 <*> desugarExpr e2
  E0Let (PCon conname pats) e1 e2 -> do
    e1' <- desugarExpr e1
    let loop letPrefix varsPrefix = \case
          [] -> return (letPrefix e2, varsPrefix [])
          PVar n : pats' -> loop letPrefix (varsPrefix . (n:)) pats'
          pat : pats' -> do
            name <- genName
            loop (letPrefix . E0Let pat (E0Var name)) (varsPrefix . (name:)) pats'
    (e2', vars) <- loop id id pats
    rhs <- desugarExpr e2'
    return $ ECase () e1' ((conname, vars, rhs) :| [])

  where
    genName :: M LName
    genName = ("x$" ++) <$> state (\i -> (show i, i + 1))
