module Parser.Parser (Parser(Parser)) where 

import Control.Monad (liftM, ap)
import Data.Functor
import Control.Applicative
import Data.Tuple

data Parser a = Parser (String -> (a, String))

parse :: Parser a -> (String -> (a, String))
parse (Parser a) = a

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure a = return a
  (<*>) = ap
    
instance Monad Parser where
  return a = Parser (\cs -> (a, cs))
  parser >>= func = Parser (\input ->
                             let parsed = parse parser input
                             in parse (func (fst parsed)) (snd parsed))

item :: Parser String
item = Parser (\input -> case input of
                    ""     -> ("", "")
                    (c:cs) -> ([c], cs))
