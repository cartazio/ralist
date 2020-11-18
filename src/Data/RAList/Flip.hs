module Data.RAList.Flip(
  module RA
  ,lookup, lookupM, lookupWithDefault, (!!)) where



import Data.Word
import qualified Prelude as P
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
lookupWithDefault = P.undefined
lookupM = P.undefined
lookup = P.undefined


