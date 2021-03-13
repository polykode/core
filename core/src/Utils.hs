{-# LANGUAGE LambdaCase #-}

module Utils where

import Config
import Control.Concurrent.ParallelIO.Local
import Text.Parsec

mapFst fn (a, b) = (fn a, b)

mapSnd fn (a, b) = (a, fn b)

getEitherWithDef def = \case
  Right x -> x
  Left _ -> def

getMaybeWithDef def = \case
  Just x -> x
  Nothing -> def

parallelIO :: [IO a] -> IO [a]
parallelIO ms = withPool lxcInitThreadPoolSize $ \tpool -> parallel tpool ms

whitespace :: Parsec String u String
whitespace = many $ oneOf [' ', '\n', '\t']

withWhitespace :: Parsec String u a -> Parsec String u a
withWhitespace comb = do
  whitespace
  content <- comb
  whitespace
  return content
