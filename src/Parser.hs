module Parser where

import Eval
import Text.ParserCombinators.Parsec

moleculeParser :: Parser Expr
moleculeParser =
    choice
        [ (char 'A' <|> char 'a') >> return (MolE A)
        , (char 'C' <|> char 'c') >> return (MolE C)
        , (char 'G' <|> char 'g') >> return (MolE G)
        , (char 'T' <|> char 't') >> return (MolE T)
        , (char 'U' <|> char 'u') >> return (MolE U)
        ]

sequenceParser :: Parser Expr
sequenceParser = do
    exprs <- many1 moleculeParser
    let mols = map toMol exprs
    pure $ SeqE mols
  where
    toMol :: Expr -> Molecule
    toMol (MolE x) = x
    toMol _ = error "not a molecule"

parseExpr :: String -> Either String Expr
parseExpr input =
    case parse sequenceParser "biolang" input of
        Left err -> Left $ "Parsing error: " ++ show err
        Right val -> Right val
