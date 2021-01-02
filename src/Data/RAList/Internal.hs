module Data.RAList.Internal where

import Data.Word
-- cribbed from indexed-traversable, modified
-- originally in  'Control.Lens.Indexed.indexed'.
newtype Indexing f a = Indexing { runIndexing :: Word64 -> (Word64, f a) }

instance Functor f => Functor (Indexing f) where
  fmap f (Indexing m) = Indexing $ \i -> case m i of
    (j, x) -> (j, fmap f x)
  {-# INLINE fmap #-}

instance Applicative f => Applicative (Indexing f) where
  pure x = Indexing $ \i -> (i, pure x)
  {-# INLINE pure #-}
  Indexing mf <*> Indexing ma = Indexing $ \i -> case mf i of
    (j, ff) -> case ma j of
                  (k, fa) -> (k, ff <*> fa)
  {-# INLINE (<*>) #-}
