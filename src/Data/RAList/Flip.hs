{-# LANGUAGE RankNTypes #-}
module Data.RAList.Flip(
  module RA
  ,lookup
  , lookupM
  , lookupWithDefault, (!!)) where



import Data.Word
--import qualified Prelude as P
import Prelude hiding (
    (++), head, last, tail, init, null, length, map, reverse,
    foldl, foldl1, foldr, foldr1, concat, concatMap,
    and, or, any, all, sum, product, maximum, minimum, take,
    drop, elem, splitAt, notElem, lookup, replicate, (!!), filter,
    zip, zipWith, unzip
    )


import Data.RAList  as RA hiding (
    (!!)
   ,lookupWithDefault
   ,lookupM
   ,lookup)
import  qualified Data.RAList as QRA


(!!) :: RAList a -> Word64 -> a
rls  !! n |  n <  0 = error "Data.RAList.Flip.!!: negative index"
                        | n >= length rls  = error "Data.RAList.Flip.!!: index too large"
                        | otherwise =  rls QRA.!! (length rls  - n )
lookupWithDefault :: forall t. t -> Word64 -> RAList t -> t
lookupWithDefault = \ def ix tree@(RAList _wt top) -> QRA.lookupWithDefault def (length tree - ix ) top

lookupM :: forall a. Word64 -> RAList a -> Either String a
lookupM = \ ix tree@(RAList _wt top) ->  QRA.lookupM (QRA.length tree  - ix) top

lookup :: forall a. Word64 -> RAList a ->a
lookup =  \ ix tree@(RAList _wt top) -> QRA.lookup (QRA.length tree - ix ) top


