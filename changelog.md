# 0.3.0.0
* changed type from
  `lookupM :: forall (m :: * -> *) a. Monad m => Word64 -> Top a -> m a` to `lookupM :: forall a. Word64 -> Top a -> Either String a`
* added `Data.RAList.Flip`, which flips the counting by having the zeroth index be the last element of the skew binary list,
rather than the head element.

# 0.2.1.1
Fix bug in log n time drop

# 0.2.1.0
Added missing traversable instance

# 0.2.0.0
updated version of ralist
includes bug fixes, api cleanup,
test suite and logarithmic drop contributed by Nell White
