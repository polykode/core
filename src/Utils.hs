{-# LANGUAGE LambdaCase #-}

module Utils where

mapFst fn (a, b) = (fn a, b)

mapSnd fn (a, b) = (a, fn b)

getEitherWithDef def = \case
  Right x -> x
  Left _ -> def

getMaybeWithDef def = \case
  Just x -> x
  Nothing -> def

liftJoin2 :: (Monad m) => (a -> b -> m c) -> m a -> m b -> m c
liftJoin2 fn m1 m2 = m1 >>= (\a -> m2 >>= fn a)

monadAppend :: Monad m => m [a] -> m a -> m [a]
monadAppend = liftJoin2 (\ls x -> return $ ls ++ [x])

concatM :: Monad m => [m a] -> m [a]
concatM = foldl monadAppend (pure [])
