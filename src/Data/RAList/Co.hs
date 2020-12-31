{-# LANGUAGE RankNTypes, DerivingVia, DeriveTraversable, PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE BangPatterns,UndecidableInstances #-}

module Data.RAList.Co(
  --module RA
  RAList(Cons,Nil,RCons,(:|),(:.))
  ,lookup
  , lookupM
  , lookupWithDefault
  , (!!)
  , lookupCC
  ,cons
  ,uncons
  --,traverse
  --,foldr
  --,foldl
  --,foldl'
  ,zip
  ,zipWith
  ,drop
  ,take

  -- * possibly useful reexports
  ,foldl'
  ,foldr
  ,traverse
  ,mapM
  ,mapM_
  ,ifoldMap
  ,imap
  ,itraverse
  ,ifoldl'
  ,ifoldr
  ,imapM

  ---
  ,(++)
  --,module Data.Traversable
  ) where



import Data.Word
--import qualified Prelude as P
import Prelude hiding (
    (++), head, last, tail, init, null, length, map, reverse,
    foldl, foldl1, foldr, foldr1, concat, concatMap,
    and, or, any, all, sum, product, maximum, minimum, take,
    drop, elem, splitAt, notElem, lookup, replicate, (!!), filter,
    zip, zipWith, unzip
    )
import Data.Foldable.WithIndex
import Data.Functor.WithIndex
import Data.Traversable.WithIndex


--import qualfieData.RAList  as RA hiding (
--    (!!)
--   ,lookupWithDefault
--   ,lookupM
--   ,lookup
--   , lookupCC )
import  qualified Data.RAList as QRA
import qualified Control.Monad.Fail as MF
import Data.Foldable
import Data.Traversable()
import GHC.Exts (IsList)

infixl 9  !!
infixr 5  `cons`, ++



newtype RAList a = CoIndex {reindex :: QRA.RAList a }
    deriving stock (Traversable)
    deriving (Foldable,Functor) via QRA.RAList
    deriving (Monoid,Semigroup,Eq,Show,IsList) via QRA.RAList a

instance Applicative RAList where
    pure = \x -> Cons x Nil
    (<*>) = zipWith ($)

infixr 5 `Cons`
pattern Cons :: forall a. a -> RAList a -> RAList a
pattern Cons x  xs <- (uncons -> Just (x,  xs ) )
    where Cons x xs =  (cons x  xs)

pattern Nil :: forall a . RAList a
pattern Nil = CoIndex QRA.Nil

{-# COMPLETE Cons, Nil #-}
infixl 5 `RCons`
pattern RCons :: forall a. RAList a -> a -> RAList a
pattern RCons xs x = Cons x xs

{-# COMPLETE RCons, Nil #-}

infixr 5 :|
pattern (:|) :: forall a. a -> RAList a -> RAList a
pattern x :| xs = Cons x xs
{-# COMPLETE (:|), Nil #-}

infixl 5 :.
pattern (:.) :: forall a. RAList a -> a -> RAList a
pattern xs :. x = Cons x xs
{-# COMPLETE (:.), Nil #-}

cons :: a -> RAList a -> RAList a
cons x (CoIndex xs) = CoIndex $  QRA.cons x xs

uncons :: RAList a -> Maybe (a, RAList a)
uncons (CoIndex xs) = case QRA.uncons xs of
                            Nothing -> Nothing
                            Just(h,rest) -> Just (h,CoIndex rest)


drop :: Word64 -> RAList a -> RAList a
drop = \ ix (CoIndex ls)-> CoIndex $ QRA.drop ix ls

take :: Word64 -> RAList a -> RAList a
take = \ix (CoIndex ls ) -> CoIndex $ QRA.take ix ls

zip :: RAList a -> RAList b -> RAList (a, b)
zip = zipWith (,)

zipWith :: (a -> b -> c ) -> RAList a -> RAList b -> RAList c
zipWith = \f (CoIndex as) (CoIndex bs) ->
              let
                !alen = QRA.wLength as
                !blen = QRA.wLength bs
                in
                  case compare alen blen of
                    EQ -> CoIndex $ QRA.zipWith f  as bs
                    GT {- alen > blen  -}->
                      CoIndex $ QRA.zipWith f  (QRA.drop (alen - blen) as)
                                               bs
                    LT {- alen < blen -} ->
                      CoIndex $ QRA.zipWith f as
                                              (QRA.drop (blen - alen ) bs)

(!!) :: RAList a -> Word64 -> a
rls  !! n |  n <  0 = error "Data.RAList.Flip.!!: negative index"
                        | n >= (fromIntegral $   length rls)  = error "Data.RAList.Flip.!!: index too large"
                        | otherwise =  reindex rls QRA.!! ((fromIntegral $length rls)  - n )
lookupWithDefault :: forall t. t -> Word64 -> RAList t -> t
lookupWithDefault = \ def ix tree -> QRA.lookupWithDefault def ((fromIntegral $ length tree) - ix ) $ reindex tree

lookupM :: forall a m . MF.MonadFail m =>  Word64 -> RAList a ->  m a
lookupM = \ ix tree ->  QRA.lookupM  (reindex tree) ((fromIntegral $ length tree)  - ix)

lookup :: forall a. Word64 -> RAList a ->a
lookup =  \ ix tree -> QRA.lookup  (reindex tree) ((fromIntegral $ length tree) - ix )

lookupCC :: RAList a -> Word64 -> (a -> r) -> (String -> r) -> r
lookupCC = \  tree ix f g ->  QRA.lookupCC (reindex tree) ((fromIntegral $ length tree) - ix ) f g
{-# INLINE wLength #-}
wLength:: RAList a -> Word64
wLength = \ (CoIndex ls) -> QRA.wLength ls
(++) :: RAList a -> RAList a -> RAList a
(++) = (<>)
