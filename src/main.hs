{-# LANGUAGE LambdaCase #-}

import qualified Data.Map as Map
import Data.Char
import Control.Monad (liftM2, liftM)
import Control.Applicative (Alternative((<|>), empty))
import Control.Monad.State
import System.Environment

type Position = (Int, Int)
type SymbolT = Map.Map String Int
type Program = (String, Position, SymbolT, String)
type Parser a = StateT Program Maybe a

item :: Parser Char
item = StateT $ \case
            ("",_,_,_) -> empty
            (c:cs, (row, col), t, os) -> pure (c, (cs, if c == '\n' 
                                                       then (row + 1, 1) 
                                                       else (row, col + 1), t, os))

setVariable :: String -> Int -> Parser ()
setVariable k v = StateT $ \(s, pos, t, os) -> pure ((), (s, pos,Map.insert k v t, os))

getVariable :: String -> Parser Int
getVariable k = StateT $ \(s, pos,t, os) -> let r = Map.lookup k t
                                            in case r of
                                            Just x -> pure (x, (s, pos, t, os))
                                            Nothing -> empty 
                                            
logOutput :: String -> Parser ()
logOutput os = StateT $ \(s, pos, t, oss) -> pure ((), (s, pos, t, oss ++ os))

sat :: (Char -> Bool) -> Parser Char
sat f = do
    c <- item
    if f c
        then pure c
        else empty

token :: Parser a -> Parser a
token p = spaces *> p <* spaces

many :: Parser a -> Parser [a]
many p = do
    x <- p
    xs <- many p
    pure (x:xs)
    <|> pure []

many1 :: Parser a -> Parser [a]
many1 p = liftM2 (:) p $ many p

space :: Parser Char
space = sat isSpace

spaces :: Parser String
spaces = many space

char :: Char -> Parser Char
char c = sat (== c) 

string :: String -> Parser String
string = foldr (liftM2 (:) . char) $ pure ""

digit :: Parser Char
digit = sat isDigit

int :: Parser Int
int = read <$> many1 digit

ident :: Parser String
ident = many1 $ sat isAlpha

output :: Parser Int
output = do
    token $ string "print"
    token $ char '('
    e <- expr
    token $ char ')'
    logOutput $ show e ++ "\n"
    pure e

expr :: Parser Int
expr = do
    x <- factor
    x' <- many $ do
        f <- addop
        f <$> factor
    pure $ foldr ($) x x'

factor :: Parser Int
factor = do
    x <- term
    x' <- many $ do
        f <- mulop
        f <$> term
    pure $ foldr ($) x x'

addop :: Parser (Int -> Int -> Int)
addop = token (char '+' >> pure (+)) <|> token (char '-' >> pure subtract) 

mulop :: Parser (Int -> Int -> Int)
mulop = (token (char '*') >> pure (*)) <|> (token (char '/') >> pure (flip div))

term :: Parser Int
term = token int 
   <|> vardecl 
   <|> (token ident >>= getVariable) 
   <|> (token (char '(') *> expr <* token (char ')'))

vardecl :: Parser Int
vardecl = do
    token $ string "let"
    s <- token ident
    token $ char '='
    e <- token expr
    setVariable s e
    pure e
    
prgm :: Parser [Int]
prgm = many1 $ do
    x <- expr <|> output
    token $ char ';'
    pure x
    
main :: IO ()
main = do
    fname <- head <$> getArgs
    s <- readFile fname
    print $ runStateT prgm (s, (1, 1), Map.empty, [])
    print $ runStateT prgm (s, (1, 1), Map.empty, "")
