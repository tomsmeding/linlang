{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Pretty (
  ppProgram,
  ppExpr,
  ppDataDef,
  ppTyp,
  ppUName,
) where

import Data.Foldable (toList)
import qualified Data.Text as T
import Prettyprinter
import Prettyprinter.Render.String

import AST


ppUName :: UName -> String
ppUName (UName lin name) = linTag lin ++ name

ppUNameD :: UName -> Doc x
ppUNameD n = ppString (ppUName n)

ppTypD :: Int -> Typ -> Doc x
ppTypD _ (TData name) = ppUNameD name
ppTypD d (TFun t1 t2) = ppParenD (d > 0) $
  ppTypD 1 t1 <+> "->" <+> ppTypD 0 t2
ppTypD d (TLinFun t1 t2) = ppParenD (d > 0) $
  ppTypD 1 t1 <+> pretty (linTag Lin) <> "->" <+> ppTypD 0 t2

ppTyp :: Int -> Typ -> String
ppTyp d t = render (ppTypD d t)

ppDataDefD :: DataDef -> Doc x
ppDataDefD (DataDef name cons) =
  "data" <+> ppUNameD name
  <+> align (sep $ zipWith (<+>) ("=" : repeat "|") (map ppCon (toList cons)))
  where
    ppCon (conname, fields) = hsep $ ppUNameD conname : map (ppTypD 1) fields

ppDataDef :: DataDef -> String
ppDataDef d = render (ppDataDefD d)

-- 0: open
-- 1: apply head
-- 2: apply arg
ppExprD :: Int -> Expr t -> Doc x
ppExprD d = \case
  EVar _ n -> ppString n
  ERecfun f t1 t2 n e -> ppParenD (d > 0) $
    hang 2 $ sep
      ["recfun" <+> ppString f <+> ":"
        <+> parens (ppTypD 1 t1 <+> "->" <+> maybe "?" (ppTypD 0) t2)
        <+> ppString n <+> "="
      ,ppExprD 0 e]
  ELinLam n t e -> ppParenD (d > 0) $
    nest 2 $ sep
      [ppString (linTag Lin) <> "λ" <> parens (ppString n <+> ":" <+> ppTypD 0 t) <+> "->"
      ,ppExprD 0 e]
  e@ELet{} -> ppParenD (d > 0) $
    let collect (ELet n e1 e2) =
          let (binds, core) = collect e2
          in ((n, e1) : binds, core)
        collect e' = ([], e')
    in let (binds, core) = collect e
    in align $ vsep $
         [hang (length prefix + 3) (sep [ppString prefix <+> ppString n <+> "="
                                        ,ppExprD 0 rhs])
         | (prefix, (n, rhs)) <- zip ("let" : repeat "in let") binds]
         ++ ["in" <+> ppExprD 0 core]
  EApp e1 e2 -> ppParenD (d > 1) $
    ppExprD 1 e1 <+> ppExprD 2 e2
  ECon _ n [] -> ppUNameD n
  ECon _ n es -> ppParenD (d > 0) $
    hsep $ ppUNameD n : map (ppExprD 2) es
  ECase _ e1 clauses -> ppParenD (d > 0) $
    nest 2 (mconcat $
      ("case" <+> ppExprD 1 e1 <+> "of")
      : [hardline <> hsep (prefix : ppUNameD n : map ppString vs) <+> "->" <+> ppExprD 0 rhs
        | (prefix, (n, vs, rhs)) <- zip ("{" : repeat ";") (toList clauses)])
    <+> "}"

ppExpr :: Int -> Expr t -> String
ppExpr d e = render (ppExprD d e)

ppProgramD :: Program t -> Doc x
ppProgramD (Program defs main) =
  case defs of
    [] -> ppMain main
    _ -> vsep $ map ppDataDefD defs ++ [emptyDoc, ppMain main]
  where
    ppMain e = nest 2 $ vsep ["main =", ppExprD 0 e]

ppProgram :: Program t -> String
ppProgram p = render (ppProgramD p)

linTag :: Linp -> String
linTag Lin = "¡"
linTag Nonlin = ""

ppString :: String -> Doc x
ppString = pretty . T.pack

ppParenD :: Bool -> Doc x -> Doc x
ppParenD True = parens
ppParenD False = id

render :: Doc x -> String
render = renderString . layoutPretty LayoutOptions { layoutPageWidth = AvailablePerLine 120 1.0 }
