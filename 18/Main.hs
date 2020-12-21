module Main where
    
-- import Text.Parsec
import Data.Either

import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Text.Parsec hiding (Empty)
import Text.Parsec.String
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language
import Text.Parsec.Expr



data Exp = Lit Integer | BinOp Char Exp Exp | Exp Exp 

instance Show Exp where
    show (Lit a) = show a
    show (BinOp op l r) = "(" ++ show l ++ " " ++ show op ++ " " ++ show r ++ ")"
    -- show (Exp e) = "(" ++ show e ++ ")"



lexerConfig = emptyDef { Token.reservedOpNames = words "* +" }

lexer = Token.makeTokenParser lexerConfig

reservedOp = Token.reservedOp lexer
integer    = Token.integer lexer

binary name label = Infix (do{ Main.reservedOp name;
                               return (\x y -> label x y)
                             }) 

opTable = [ [ binary "+" (BinOp '+') AssocLeft
            , binary "*" (BinOp '*') AssocLeft ] ]            


opTable2 = [ [ binary "+" (BinOp '+') AssocLeft],
             [ binary "*" (BinOp '*') AssocLeft ] ] 

expr :: Parser Exp
expr = buildExpressionParser opTable term

expr2 :: Parser Exp
expr2 = buildExpressionParser opTable2 term2

parens = Token.parens lexer  

term :: Parser Exp
term =  value
    <|> Main.parens expr 

term2 :: Parser Exp
term2 =  value
    <|> Main.parens expr2

value :: Parser Exp
value = do Lit <$> Main.integer

solveExp :: Exp -> Integer
solveExp (Lit a) = a
solveExp (BinOp '+' a b) = solveExp a + solveExp b
solveExp (BinOp '*' a b) = solveExp a * solveExp b

fromRightOrFail :: Either a b -> b
fromRightOrFail (Right b) = b

parseFromLine :: Parser Exp -> String -> Either ParseError Exp
parseFromLine e s = parse (e <* eof) "" s

main :: IO ()
main = do
  content <- readFile "input_18a.txt"
  let expressions = rights $ map (parseFromLine expr) $ lines content
      result1 = sum $ map solveExp expressions
  print result1
  let expressions = rights $ map (parseFromLine expr2) $ lines content
      result2 = sum $ map solveExp expressions
  print result2
