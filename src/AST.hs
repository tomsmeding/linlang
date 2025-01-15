{-# LANGUAGE LambdaCase #-}
module AST where

import Data.List.NonEmpty (NonEmpty)


data Linp = Lin | Nonlin
  deriving (Show, Eq, Ord)

data UName = UName Linp String  -- data types and constructors
  deriving (Show, Eq, Ord)

type LName = String  -- variables

data Typ = TData UName
         | TFun Typ Typ
         | TLinFun Typ Typ
  deriving (Show, Eq)

data DataDef = DataDef UName (NonEmpty (UName, [Typ]))
  deriving (Show)

-- | Internal language
data Expr t
  = EVar t LName
  | ERecfun LName Typ (Maybe Typ) LName (Expr t)  -- nonlinear
  | ELinLam LName Typ (Expr t)
  | ELet LName (Expr t) (Expr t)
  | EApp (Expr t) (Expr t)
  | ECon t UName [Expr t]
  | ECase t (Expr t) (NonEmpty (UName, [LName], Expr t))
  deriving (Show)

-- | Surface-language expression
data Expr0
  = E0Var LName
  | E0Recfun LName Typ LName Expr0
  | E0Lam Linp LName Typ Expr0
  | E0Let Pattern Expr0 Expr0
  | E0App Expr0 Expr0
  | E0Con UName [Expr0]
  | E0Case Expr0 (NonEmpty (UName, [LName], Expr0))
  deriving (Show)

data Pattern
  = PWild
  | PVar LName
  | PCon UName [Pattern]
  deriving (Show)

data Program t = Program { progDatas :: [DataDef], progMain :: Expr t }
  deriving (Show)

data Program0 = Program0 [DataDef] Expr0
  deriving (Show)

typeOf :: Expr Typ -> Typ
typeOf = \case
  EVar t _ ->
    t
  ERecfun _ t1 _ _ e ->
    TFun t1 (typeOf e)
  ELinLam _ t1 e ->
    TLinFun t1 (typeOf e)
  ELet _ _ e ->
    typeOf e
  EApp e1 _ ->
    case typeOf e1 of
      TLinFun _ t2 -> t2
      TFun _ t2 -> t2
      _ -> error $ "typeOf: EApp of non-function: " ++ show (typeOf e1)
  ECon t _ _ -> t
  ECase t _ _ -> t

isLinear :: Typ -> Bool
isLinear (TData (UName Lin _)) = True
isLinear (TData (UName Nonlin _)) = False
isLinear TLinFun{} = True
isLinear TFun{} = False
