{-# LANGUAGE RankNTypes, DerivingVia, DeriveTraversable, PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE BangPatterns,UndecidableInstances,MultiParamTypeClasses #-}
{-# LANGUAGE MonadComprehensions #-}

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

import Control.Applicative.Backwards

import Data.RAList.Internal
-- provides indexing applicative

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
import Control.Monad.Zip
import Data.Coerce
import GHC.Generics(Generic,Generic1)

import Control.Applicative(Applicative(liftA2))


infixl 9  !!
infixr 5  `cons`, ++

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



newtype RAList a = CoIndex {reindex :: QRA.RAList a }
    deriving stock (Traversable)
    deriving (Foldable,Functor,Generic1) via QRA.RAList
    deriving (Monoid,Semigroup,Eq,Ord,Show,IsList,Generic) via QRA.RAList a

--- > itraverse (\ix _val -> Id.Identity ix) $ ([(),(),(),()]:: Co.RAList ())
--- Identity (fromList [3,2,1,0])
instance   TraversableWithIndex Word64 RAList where
  {-# INLINE itraverse #-}
  itraverse = \ f s -> snd $ runIndexing
                ( forwards $  traverse (\a -> Backwards $ Indexing (\i -> i `seq` (i + 1, f i a))) s) 0
-- TODO; benchmark this vs counting downn from the start

instance   FoldableWithIndex Word64 RAList where
instance   FunctorWithIndex Word64 RAList where


instance Applicative RAList where
    {-# INLINE pure #-}
    pure = \x -> Cons x Nil
    {-# INLINE (<*>) #-}
    fs <*> xs = [f x | f <- fs, x <- xs]
    {-# INLINE liftA2 #-}
    liftA2 f xs ys = [f x y | x <- xs, y <- ys]
    {-# INLINE (*>) #-}
    xs *> ys  = [y | _ <- xs, y <- ys]

instance Monad RAList where
    return = pure
    (>>=) = (\ls f -> CoIndex $ QRA.concatMap (\ x -> coerce $ f x)   $ reindex ls   )



--- QUESTION --- am i wrong for using the Ziplist applicative with my monads?


{-



if we have <*> === zipWith ($)
that means we need to have the monad be the DIAGONLIZATION rather than concat map



we need  ap === <*>

ap                :: (Monad m) => m (a -> b) -> m a -> m b
ap m1 m2          = do { x1 <- m1; x2 <- m2; return (x1 x2) }
-- Since many Applicative instances define (<*>) = ap, we
-- cannot define ap = (<*>)
-}
instance MonadZip RAList where
  mzipWith = zipWith
  munzip = unzip

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

{-# INLINE unzip #-}
-- adapted from List definition in base
-- not perfectly certain about  being lazy on the *rest*
-- but lets leave it for now... though i think my cons
-- algorithm precludes it from actually being properly lazy
-- TODO : mess with foldr' vs foldr and ~ vs ! for as and bs from unzip definition
unzip :: RAList (a,b) -> (RAList a,RAList b)
unzip    =  foldr' (\(a,b) (!as,!bs) -> (a:| as,b:|bs)) (Nil,Nil)

--unzip    =  foldr (\(a,b) ~(as,bs) -> (a:| as,b:|bs)) (Nil,Nil)

--- this zipWith has better efficiency  than the opposite one
-- in the case of differing  length RALists, because we can drop from the front
-- efficiently but not from the back!
-- we need to do this flip around
--- this semantic arise from counting the indexing from the rear in this module
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
{-# INLINE (!!) #-}
(!!) :: RAList a -> Word64 -> a
rls  !! n |  n <  0 = error "Data.RAList.Flip.!!: negative index"
                        | n >= (wLength  rls)  = error "Data.RAList.Flip.!!: index too large"
                        | otherwise =  reindex rls QRA.!! ((wLength rls)  - n )
{-# INLINE lookupWithDefault #-}
lookupWithDefault :: forall t. t -> Word64 -> RAList t -> t
lookupWithDefault = \ def ix tree -> QRA.lookupWithDefault def ((wLength tree) - ix ) $ reindex tree


{-# INLINE lookupM #-}
lookupM :: forall a m . MF.MonadFail m =>  Word64 -> RAList a ->  m a
lookupM = \ ix tree ->  QRA.lookupM  (reindex tree) ((wLength tree)  - ix)

{-# INLINE lookup #-}
lookup :: forall a. Word64 -> RAList a -> Maybe a
lookup =  \ ix tree -> QRA.lookup  (reindex tree) ((wLength tree) - ix )

{-# INLINE lookupCC #-}
lookupCC :: RAList a -> Word64 -> (a -> r) -> (String -> r) -> r
lookupCC = \  tree ix f g ->  QRA.lookupCC (reindex tree) ((wLength tree) - ix ) f g

{-# INLINE wLength #-}
wLength:: RAList a -> Word64
wLength = \ (CoIndex ls) -> QRA.wLength ls

(++) :: RAList a -> RAList a -> RAList a
(++) = (<>)
