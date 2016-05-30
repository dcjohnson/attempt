module Parser.Parser (Parser(Parser)) where 

import Control.Monad (liftM, ap)
import Data.Functor
import Control.Applicative

data Parser a = Parser (String -> [(a, String)])

parse :: Parser a -> (String -> [(a, String)])
parse (Parser a) = a

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure a = return a
  (<*>) = ap
    
instance Monad Parser where
  return a = Parser (\cs -> [(a, cs)])
  parser >>= func = Parser (\cs -> concat [parse (func a) cs | (a, cs) <- parse parser cs])
