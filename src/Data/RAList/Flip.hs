{-# LANGUAGE RankNTypes #-}
module Data.RAList.Flip(
  module RA
  ,lookup
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


import Data.RAList  as RA hiding (
    (!!)
   ,lookupWithDefault
   ,lookupM
   ,lookup
   , lookupCC )
import  qualified Data.RAList as QRA
import qualified Control.Monad.Fail as MF

(!!) :: RAList a -> Word64 -> a
rls  !! n |  n <  0 = error "Data.RAList.Flip.!!: negative index"
                        | n >= (fromIntegral $   length rls)  = error "Data.RAList.Flip.!!: index too large"
                        | otherwise =  rls QRA.!! ((fromIntegral $length rls)  - n )
lookupWithDefault :: forall t. t -> Word64 -> RAList t -> t
lookupWithDefault = \ def ix tree -> QRA.lookupWithDefault def ((fromIntegral $ length tree) - ix ) tree

lookupM :: forall a m . MF.MonadFail m =>  Word64 -> RAList a ->  m a
lookupM = \ ix tree ->  QRA.lookupM  tree ((fromIntegral $ QRA.length tree)  - ix)

lookup :: forall a. Word64 -> RAList a ->a
lookup =  \ ix tree -> QRA.lookup  tree ((fromIntegral $ QRA.length tree) - ix )

lookupCC :: RAList a -> Word64 -> (a -> r) -> (String -> r) -> r
lookupCC = \  tree ix f g ->  QRA.lookupCC tree ((fromIntegral $ QRA.length tree) - ix ) f g
