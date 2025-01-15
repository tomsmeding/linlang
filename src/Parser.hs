module Parser (parseProgram) where

import Control.Applicative (asum)
import Control.Monad
import Data.Char
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty(..))
import Text.Parsec

import AST


parseProgram :: FilePath -> String -> Either String Program0
parseProgram path source =
  case parse pProgram path source of
    Right res -> Right res
    Left err -> Left $
      let line = case drop (sourceLine (errorPos err) - 1) (lines source) of
                   [] -> ""
                   l:_ -> l
          lnum = show (sourceColumn (errorPos err))
          lnumlen = length lnum
      in show err ++ "\n" ++
         replicate lnumlen ' ' ++ " |\n" ++
         lnum ++ " | " ++ line ++ "\n" ++
         replicate lnumlen ' ' ++ " | " ++ replicate (sourceColumn (errorPos err) - 1) ' ' ++ "^"

type Parser = Parsec String ()

pProgram :: Parser Program0
pProgram = do
  defs <- many pDataDef
  skipR
  keyword0 "main"
  symbol "="
  e <- pExpr
  return (Program0 defs e)

pTyp :: Parser Typ
pTyp = do
  skipI
  t1 <- pTypAtom
  asum [do lin <- try $ pSomeLin <* string "->"
           t2 <- pTyp
           return (case lin of Lin -> TLinFun t1 t2
                               Nonlin -> TFun t1 t2)
       ,return t1]

pTypAtom :: Parser Typ
pTypAtom = asum
  [do symbol "("
      t <- pTyp
      symbol ")"
      return t
  ,TData <$> pUpName]

pDataDef :: Parser DataDef
pDataDef = do
  try $ skipR >> keyword0 "data"
  name <- pUpName
  symbol "="
  con1 <- pDataDefCon
  cons <- many (symbol "|" >> pDataDefCon)
  return (DataDef name (con1 :| cons))
  where
    pDataDefCon = do
      name <- pUpName
      fields <- many pTypAtom
      return (name, fields)

pExpr :: Parser Expr0
pExpr = pELam <|> pRecfun <|> pECase <|> pELet <|> pECon <|> pEApps
  where
    pELam = do
      lin <- try $ pSomeLin <* oneOf "\\λ"
      symbol "("
      var <- pLoName
      symbol ":"
      t <- pTyp
      symbol ")"
      symbol "->"
      e <- pExpr
      return (E0Lam lin var t e)

    pRecfun = do
      keyword "recfun"
      name <- pLoName
      symbol ":"
      symbol "("
      funty <- pTyp
      symbol ")"
      argname <- pLoName
      symbol "="
      e <- pExpr
      return (E0Recfun name funty argname e)

    pECase = do
      keyword "case"
      e1 <- pExpr
      keyword "of"
      symbol "{"
      clause1 <- pClause
      clauses <- many (symbol ";" >> pClause)
      symbol "}"
      return (E0Case e1 (clause1 :| clauses))
      where
        pClause = do
          cname <- pUpName
          vars <- many pLoName
          symbol "->"
          e <- pExpr
          return (cname, vars, e)

    pELet = do
      keyword "let"
      pat <- pPattern
      symbol "="
      e1 <- pExpr
      keyword "in"
      e2 <- pExpr
      return (E0Let pat e1 e2)

    pECon = do
      name <- pUpName
      es <- many pExprAtom
      return (E0Con name es)

    pEApps :: Parser Expr0
    pEApps = do
      e <- pExprAtom
      es <- many pExprAtom
      return (foldl' E0App e es)

pExprAtom :: Parser Expr0
pExprAtom =
  asum [do symbol "("
           e <- pExpr
           symbol ")"
           return e
       ,E0Var <$> pLoName
       ,E0Con <$> pUpName <*> pure []]

pPattern :: Parser Pattern
pPattern = pPCon <|> pPatternAtom
  where
    pPCon = PCon <$> pUpName <*> many pPatternAtom

    pPatternAtom = pPWild <|> pPVar
    pPWild = PWild <$ keyword "_"
    pPVar = PVar <$> pLoName

pLoName :: Parser LName
pLoName = try $ do
  skipI
  name <- (:) <$> satisfy isLower <*> pNameTail0
  guard (name `notElem` reservedWords)
  return name

pUpName0, pUpName :: Parser UName
pUpName0 = UName <$> pSomeLin0 <*> ((:) <$> satisfy isUpper <*> pNameTail0)
pUpName = try $ skipI >> pUpName0

pNameTail0 :: Parser String
pNameTail0 = many (satisfy isInnerWordChar)

pSomeLin, pSomeLin0 :: Parser Linp
pSomeLin = skipI >> pSomeLin0
pSomeLin0 = Lin <$ pLin0 Lin <|> pure Nonlin

pLin0 :: Linp -> Parser ()
pLin0 Lin = () <$ oneOf "¡#"
pLin0 Nonlin = return ()

symbol :: String -> Parser ()
symbol s = try (skipI >> string s) >> return ()

keyword :: String -> Parser ()
keyword s = try $ skipI >> keyword0 s

keyword0 :: String -> Parser ()
keyword0 s = try (string s >> notFollowedBy (satisfy isInnerWordChar))

reservedWords :: [String]
reservedWords = ["case", "of", "let", "in", "recfun"]

isInnerWordChar :: Char -> Bool
isInnerWordChar c = isAlphaNum c || c `elem` "_'"

skipI, skipR :: Parser ()
skipI = try $ whitecomment >> gIndent
skipR = try $ whitecomment >> gRoot

gIndent :: Parser ()
gIndent = do
  pos <- getPosition
  guard (sourceColumn pos > 1)

gRoot :: Parser ()
gRoot = do
  pos <- getPosition
  guard (sourceColumn pos == 1)

whitecomment :: Parser ()
whitecomment = do
  spaces
  asum [do _ <- try (string "-- ") >> manyTill anyChar newline
           whitecomment
       ,return ()]
