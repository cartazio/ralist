{-# LANGUAGE RankNTypes, DerivingVia #-}
module Data.RAList.Co(
  --module RA
  lookup
  , lookupM
  , lookupWithDefault, (!!), lookupCC) where



import Data.Word
--import qualified Prelude as P
import Prelude hiding (
    (++), head, last, tail, init, null, length, map, reverse,
    foldl, foldl1, foldr, foldr1, concat, concatMap,
    and, or, any, all, sum, product, maximum, minimum, take,
    drop, elem, splitAt, notElem, lookup, replicate, (!!), filter,
    zip, zipWith, unzip
    )


--import qualfieData.RAList  as RA hiding (
--    (!!)
--   ,lookupWithDefault
--   ,lookupM
--   ,lookup
--   , lookupCC )
import  qualified Data.RAList as QRA
import qualified Control.Monad.Fail as MF
import Data.Foldable




newtype RAList a = CoIndex {reindex :: QRA.RAList a }
     deriving (Foldable,Functor) via QRA.RAList
     deriving (Monoid,Semigroup,Eq,Show) via QRA.RAList a
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
