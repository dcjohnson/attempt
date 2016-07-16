module Parser.Parser (Parser(Parser)) where

-- This is not an origional idea.
-- The basis of this code is this paper.
-- http://research.microsoft.com/en-us/um/people/daan/download/papers/parsec-paper.pdf

import Control.Monad (liftM, ap)
import Data.Functor
import Control.Applicative
import Data.Tuple

data Parser a = Parser (String -> Consumed a)
data Consumed a = Consumed (Reply a) | Empty (Reply a)
data Reply a = Ok a String | Error

getParseFunc :: Parser a -> (String -> Consumed a)
getParseFunc (Parser a) = a

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure a = return a
  (<*>) = ap

instance Monad Parser where
  return a = Parser(\input -> Empty (Ok a input))
  parser >>= applyFunc = Parser (\input -> case ((getParseFunc parser) input) of
                                    Consumed reply1 -> Consumed (case reply1 of
                                                          Ok val rest -> case ((getParseFunc (applyFunc val)) rest) of
                                                                           Consumed reply2 -> reply2
                                                                           Empty reply2    -> reply2
                                                          Error       -> Error)
                                    Empty reply1    -> case reply1 of
                                                       Ok val rest -> ((getParseFunc (applyFunc val)) rest)
                                                       Error       -> Empty Error)
