{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExplicitForAll, RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms,ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFoldable , DeriveTraversable#-}
-- |
-- A random-access list implementation based on Chris Okasaki's approach
-- on his book \"Purely Functional Data Structures\", Cambridge University
-- Press, 1998, chapter 9.3.
--
-- 'RAList' is a replacement for ordinary finite lists.
-- 'RAList' provides the same complexity as ordinary for most the list operations.
-- Some operations take /O(log n)/ for 'RAList' where the list operation is /O(n)/,
-- notably indexing, '(!!)'.
--
module Data.RAList
   (
     RAList(Nil,Cons)

   -- * Basic functions
   , empty
   , cons
   , uncons
--   , singleton
   , (++)
   , head
   , last
   , tail
   , init
   , null
   , length

   -- * Indexing lists
   -- | These functions treat a list @xs@ as a indexed collection,
   -- with indices ranging from 0 to @'length' xs - 1@.

   , (!!)
   ,lookupWithDefault
   ,lookupM
   ,lookup
   ,lookupCC

   --- * KV indexing
   --- | This function treats a RAList as an association list
   ,lookupL


   -- * List transformations
   , map
   , reverse
{-RA
   , intersperse
   , intercalate
   , transpose

   , subsequences
   , permutations

   -- * Reducing lists (folds)
-}
   , foldl
   , foldl'
   , foldl1
   , foldl1'
   , foldr
   , foldr1

   -- ** Special folds

   , concat
   , concatMap
   , and
   , or
   , any
   , all
   , sum
   , product
   , maximum
   , minimum

   -- * Building lists
{-RA
   -- ** Scans
   , scanl
   , scanl1
   , scanr
   , scanr1

   -- ** Accumulating maps
   , mapAccumL
   , mapAccumR
-}
   -- ** Repetition
   , replicate

{-RA
   -- ** Unfolding
   , unfoldr
-}

   -- * Sublists

   -- ** Extracting sublists
   , take
   , drop
   , simpleDrop
   , splitAt
{-RA

   , takeWhile
   , dropWhile
   , dropWhileEnd
   , span
   , break

   , stripPrefix

   , group

   , inits
   , tails

   -- ** Predicates
   , isPrefixOf
   , isSuffixOf
   , isInfixOf
-}
   -- * Searching lists

   -- ** Searching by equality
   , elem
   , notElem

{-RA
   -- ** Searching with a predicate
   , find
-}
   , filter
   , partition

{-RA
   , elemIndex
   , elemIndices

   , findIndex
   , findIndices
-}
   -- * Zipping and unzipping lists

   , zip
{-RA
   , zip3
   , zip4, zip5, zip6, zip7
-}
   , zipWith
{-RA
   , zipWith3
   , zipWith4, zipWith5, zipWith6, zipWith7
-}
   , unzip
{-RA
   , unzip3
   , unzip4, unzip5, unzip6, unzip7

   -- * Special lists

   -- ** Functions on strings
   , lines
   , words
   , unlines
   , unwords

   -- ** \"Set\" operations

   , nub

   , delete
   , (\\)

   , union
   , intersect

   -- ** Ordered lists
   , sort
   , insert

   -- * Generalized functions

   -- ** The \"@By@\" operations

   -- *** User-supplied equality (replacing an @Eq@ context)
   -- | The predicate is assumed to define an equivalence.
   , nubBy
   , deleteBy
   , deleteFirstsBy
   , unionBy
   , intersectBy
   , groupBy

   -- *** User-supplied comparison (replacing an @Ord@ context)
   -- | The function is assumed to define a total ordering.
   , sortBy
   , insertBy
   , maximumBy
   , minimumBy

   -- ** The \"@generic@\" operations
   -- | The prefix \`@generic@\' indicates an overloaded function that
   -- is a generalized version of a "Prelude" function.

   , genericLength
   , genericTake
   , genericDrop
   , genericSplitAt
   , genericIndex
   , genericReplicate
-}
   -- * Update
   , update
   , adjust
   -- * List conversion
   , toList
   , fromList
   -- * List style fusion tools
   , build
   , unfoldr
   , augment
   , genericLength
   , wLength
   ) where
import qualified Prelude
import Prelude hiding(
    (++), head, last, tail, init, null, length, map, reverse,
    foldl, foldl1, foldr, foldr1, concat, concatMap,
    and, or, any, all, sum, product, maximum, minimum, take,
    drop, elem, splitAt, notElem, lookup, replicate, (!!), filter,
    zip, zipWith, unzip
    )
import qualified Data.List as List

-- this should be a cabal flag for debugging data structure bugs :)
#define DEBUG 0

#if MIN_VERSION_base(4,11,0)
#else
import Data.Semigroup(Semigroup,(<>))
#endif
import Data.Data(Data,Typeable)
--import Data.Functor.Identity(runIdentity)
import Data.Word

import Data.Foldable hiding (concat, concatMap)
import qualified Control.Monad.Fail as MF

import Control.Monad.Zip

infixl 9  !!
infixr 5  `cons`, ++

-- A RAList is stored as a list of trees.  Each tree is a full binary tree.
-- The sizes of the trees are monotonically increasing, except that the two
-- first trees may have the same size.
-- The first few tree sizes:
-- [ [], [1], [1,1], [3], [1,3], [1,1,3], [3,3], [7], [1,7], [1,1,7],
--   [3,7], [1,3,7], [1,1,3,7], [3,3,7], [7,7], [15], ...
-- (I.e., skew binary numbers.)


#if !DEBUG
instance (Show a) => Show (RAList a) where
    showsPrec p xs = showParen (p >= 10) $ showString "fromList " . showsPrec 10 (toList xs)
#endif

--instance (Read a) => Read (RAList a) where
--    readsPrec p = readParen (p > 10) $ \ r -> [(fromList xs, t) | ("fromList", s) <- lex r, (xs, t) <- reads s]

--instance (Ord a) => Ord (RAList a) where
--    xs <  ys        = toList xs <  toList ys
--    xs <= ys        = toList xs <= toList ys
--    xs >  ys        = toList xs >  toList ys
--    xs >= ys        = toList xs >= toList ys
--    xs `compare` ys = toList xs `compare` toList ys

instance Monoid (RAList a) where
    mempty  = Nil

#if MIN_VERSION_base(4,11,0)

#elif MIN_VERSION_base(4,9,0)
    mappend = (<>)
#endif

instance Semigroup (RAList a) where
    (<>) = (++)

--instance Functor RAList where
--    fmap f (RAList s skewlist) = RAList s (fmap f skewlist)

instance Applicative RAList where
    pure = \x -> Cons x Nil
    (<*>) = zipWith ($)

instance Monad RAList where
    return = pure
    (>>=) = flip concatMap

instance MonadZip RAList where
  mzipWith = zipWith

-- Special list type for (Word64, Tree a), i.e., Top a ~= [(Word64, Tree a)]
data RAList a = RNil
                | RCons {-# UNPACK #-}  !Word64 -- total number of elements, aka sum of subtrees
                        {-# UNPACK #-}  !Word64 --  size of this subtree
                                        (Tree a)
                                        (RAList a)
    deriving (Eq
              ,Data
              ,Typeable
              ,Functor
              ,Traversable
#if DEBUG
              , Show
#endif
              )

instance Foldable RAList  where
  null Nil  =  True
  null _ = False

  length RNil = 0
  length (RCons tot _tres _tree _rest) = fromIntegral tot -- :)
  foldMap _f RNil = mempty
#if MIN_VERSION_base(4,11,0)
  foldMap f (RCons _stot _stre tree rest) = foldMap f tree <> foldMap f rest
#else
  foldMap f (RCons _stot _stre tree rest) = foldMap f tree `mappend` foldMap f rest
#endif

--instance Functor Top where
--    fmap _ Nil = Nil
--    fmap f (Cons w t xs) = Cons w (fmap f t) (fmap f xs)

-- Complete binary tree.  The completeness of the trees is an invariant that must
-- be preserved for the implementation to work.

{-# specialize genericLength :: RAList a -> Word64  #-}
{-# specialize genericLength :: RAList a -> Integer  #-}
{-# specialize genericLength :: RAList a -> Int  #-}
{-# specialize genericLength :: RAList a -> Word  #-}
genericLength :: Integral w =>RAList a -> w
genericLength = \ ra -> case ra of RNil ->  0 ; (RCons tot _trtot _tree _rest) -> fromIntegral tot

wLength :: RAList a -> Word64
wLength = genericLength

data Tree a
     = Leaf a
     | Node a (Tree a) (Tree a)
     deriving
        (Eq
        ,Data
        ,Typeable
        ,Functor
        ,Traversable
#if DEBUG
        , Show
#endif
         )

instance Foldable Tree  where
  -- Tree is a PREORDER sequence layout
  foldMap f (Leaf a) = f a
#if MIN_VERSION_base(4,11,0)
  foldMap f (Node a l r) =  f a <> foldMap f l <>  foldMap f r
#else
  foldMap f (Node a l r) =  f a `mappend` foldMap f l `mappend`  foldMap f r
#endif
--instance Functor Tree where
--     fmap f (Leaf x)     = Leaf (f x)
--     fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)


pattern Nil :: forall a. RAList a
pattern Nil = RNil

pattern Cons :: forall a. a -> RAList a -> RAList a
pattern Cons x xs <-( uncons -> Just(x,xs) )
 where Cons x xs = cons x xs
{-# COMPLETE Nil,Cons#-}

empty :: RAList a
empty = Nil

-- | Complexity /O(1)/.
cons :: a -> RAList a -> RAList a
cons x (RCons tots1 tsz1 t1
              (RCons _tots2 tsz2 t2 rest))
           | tsz2 == tsz1 = RCons (tots1 + 1) (tsz1 * 2 + 1 ) (Node x t1 t2 ) rest
cons x rlist  = RCons (1 + wLength rlist ) 1 (Leaf x) rlist

(++) :: RAList a -> RAList a -> RAList a
xs  ++ Nil = xs
Nil ++ ys = ys
xs  ++ ys = foldr cons ys xs


uncons :: RAList a -> Maybe (a, RAList a)
uncons (RNil) =  Nothing
uncons (RCons _tot _treetot  (Leaf h)     wts) =  Just (h,wts)
uncons (RCons _tot w (Node x l r) wts) = Just (x, (RCons (restsize + w2 + w2) w2 l (RCons (restsize + w2) w2 r wts)))
      where
        w2 = w `quot` 2
        restsize = wLength wts

-- | Complexity /O(1)/.
head :: RAList a -> Maybe a
head = fmap fst  . uncons

-- | Complexity /O(log n)/.
last :: RAList a -> a
last xs= xs !! (genericLength xs - 1)

half :: Word64 -> Word64
half = \ n ->  n `quot` 2

-- | Complexity /O(log n)/.
(!!) :: RAList a -> Word64 -> a
r !! n | n <  0 = error "Data.RAList.!!: negative index"
                    | n >=( genericLength r)  = error "Data.RAList.!!: index too large"
                    | otherwise = lookupCC  r n  id error


lookupCC :: forall a r.  RAList a ->  Word64 -> (a -> r) -> (String -> r) -> r
lookupCC  =  \  ralist  index  retval retfail ->
    let
                look RNil _ = retfail "RAList.lookup bad subscript, something is corrupted"
                look (RCons _tots tsz t xs) ix
                    | ix < tsz     = lookTree tsz  ix t
                    | otherwise = look xs (ix - tsz)

                lookTree _  ix (Leaf x)
                    | ix == 0    = retval x
                    | otherwise = retfail "RAList.lookup: not found. somehow we reached a leaf but our index doesnt match, this is bad"
                lookTree jsz ix (Node x l r)
                    | ix > (half jsz)  = lookTree (half jsz) (ix - 1 - (half jsz)) r
                    | ix /= 0        = lookTree (half jsz) (ix - 1) l -- ix between zero and floor of size/2
                    | otherwise     = retval x  -- when ix is zero
      in
        if  index >= (genericLength  ralist)
           then  retfail $   "provide index larger than Ralist max valid coord " <> (show index) <> " " <> (show (length ralist))
           else look ralist index


-- todo before release: rewrite in terms of lookupCC
lookup :: forall a. RAList a ->  Word64 -> a
lookup  = \ xs i -> maybe (error  "woops") id (lookupM xs i)




{-# SPECIALIZE lookupM :: forall a . RAList a ->  Word64 -> Maybe a  #-}
{-# SPECIALIZE lookupM :: forall a . RAList a ->  Word64 ->  IO a  #-}
lookupM :: forall a m. MF.MonadFail m => RAList a ->   Word64 -> m  a
lookupM = \ ix lst  -> lookupCC ix lst return fail

lookupWithDefault :: forall t. t -> Word64 ->  RAList t ->   t
lookupWithDefault = \  d tree ix  -> lookupCC  ix tree id (const d)


-- | Complexity /O(1)/.
tail :: RAList a -> Maybe (RAList a)
tail = fmap snd . uncons
-- XXX Is there some clever way to do this?
init :: RAList a -> RAList a
init = fromList . Prelude.init . toList


-- -- | Complexity /O(1)/.
--length :: RAList a -> Word64
--length (RCons s  _treesize _tree  _rest) = s
--length RNil = 0

map :: (a->b) -> RAList a -> RAList b
map = fmap


-- | 'reverse' @xs@ returns the elements of @xs@ in reverse order.
-- @xs@ must be finite.
reverse                 :: RAList a -> RAList a
#if defined(USE_REPORT_PRELUDE)
reverse                 =  foldl (flip  cons) Nil
#else
reverse l =  rev l Nil
  where
    rev Nil    a = a
    rev (Cons x xs) a = rev xs (Cons x a)
#endif



foldl1' :: (a -> a -> a) -> RAList a -> a
foldl1' f xs | null xs = errorEmptyList "foldl1'"
             | otherwise = List.foldl1' f (toList xs)

---- XXX This could be deforested.
--foldr :: (a -> b -> b) -> b -> RAList a -> b
--foldr f z xs = Prelude.foldr f z (toList xs)

--foldr1 :: (a -> a -> a) -> RAList a -> a
--foldr1 f xs | null xs = errorEmptyList "foldr1"
--            | otherwise = Prelude.foldr1 f (toList xs)

concat :: RAList (RAList a) -> RAList a
concat = foldr (<>) empty

concatMap :: (a -> RAList b) -> RAList a -> RAList b
concatMap f = concat . fmap f

--and :: RAList Bool -> Bool
--and = foldr (&&) True

--or :: RAList Bool -> Bool
--or = foldr (||) False

--any :: (a -> Bool) -> RAList a -> Bool
--any p = or . map p

--all :: (a -> Bool) -> RAList a -> Bool
--all p = and . map p

--sum :: (Num a) => RAList a -> a
--sum = foldl (+) 0

--product :: (Num a) => RAList a -> a
--product = foldl (*) 1

--maximum :: (Ord a) => RAList a -> a
--maximum xs | null xs   = errorEmptyList "maximum"
--           | otherwise = foldl1 max xs

--minimum :: (Ord a) => RAList a -> a
--minimum xs | null xs   = errorEmptyList "minimum"
--           | otherwise = foldl1 min xs

replicate :: Word64 -> a -> RAList a
replicate n v = fromList $ Prelude.replicate (fromIntegral n)  v

take :: Word64 -> RAList a -> RAList a
take n ls | n < fromIntegral (maxBound :: Int) = fromList $  Prelude.take (fromIntegral n) $ toList ls
          | otherwise = ls

-- | drop i l
-- @`drop` i l@ where l has length n has worst case complexity  Complexity /O(log n)/, Average case
-- complexity should be /O(min(log i, log n))/.
drop :: Word64 -> RAList a -> RAList a
drop n xs | n <= 0 = xs
drop n  rlist  | n >=( fromIntegral $ length rlist) = Nil
drop n rlist  = (loop n rlist)
  where loop 0 xs = xs
        loop m (RCons _tot treesize _ xs) | treesize <= m = loop (m-treesize) xs -- drops full trees
        loop m (RCons _tot treesize  tre xs) = splitTree m treesize tre xs -- splits tree
        loop _ _ = error "Data.RAList.drop: impossible"

-- helper function for drop
-- drops the first n elements of the tree and adds them to the front
splitTree :: Word64 -> Word64 -> Tree a -> RAList a -> RAList a
splitTree n treeSize tree@(Node _ l r) xs =
    case (compare n  1, n <= half treeSize) of
      (LT {- n==0 -}, _ )  -> RCons (suffixSize + treeSize)  treeSize tree xs
      (EQ {- n==1 -}, _ ) -> RCons (suffixSize + 2* halfTreeSize) halfTreeSize l
                                (RCons (suffixSize + halfTreeSize) halfTreeSize r xs)
      (_, True ) -> splitTree (n-1) halfTreeSize l (RCons (suffixSize + halfTreeSize) halfTreeSize r xs)
      (_, False) -> splitTree (n-halfTreeSize-1) halfTreeSize r xs
    where suffixSize = genericLength xs
          halfTreeSize = half treeSize
splitTree n treeSize nd@(Leaf _) xs =
  case compare n 1 of
    EQ {-1-} -> xs
    LT {-0-}-> RCons ((genericLength xs) + treeSize) treeSize nd xs
    GT {- > 1-} -> error "drop invariant violated, must be smaller than current tree"




-- Old version of drop
-- worst case complexity /O(n)/
simpleDrop :: Word64 -> RAList a -> RAList a
simpleDrop n xs  | n <= 0 = xs
                 | n >= (genericLength xs) = Nil
                 | otherwise =  (loop n xs)
    where loop 0 rs = rs
          loop m (RCons _tot w _ rs) | w <= m = loop (m-w) rs
          loop m (RCons _tot w (Node _ l r) rs) = loop (m-1) (RCons ((genericLength xs) + 2 * w2) w2 l (RCons ((genericLength xs) + w2) w2 r rs))
            where w2 = half w
          loop _ _ = error "Data.RAList.drop: impossible"


splitAt :: Word64 -> RAList a -> (RAList a, RAList a)
splitAt n xs = (take n xs, drop n xs)

--elem :: (Eq a) => a -> RAList a -> Bool
--elem x = any (== x)

--notElem :: (Eq a) => a -> RAList a -> Bool
--notElem x = not . elem x -- aka all (/=)

-- naive list based lookup
lookupL :: (Eq a) => a -> RAList (a, b) -> Maybe b
lookupL x xys = Prelude.lookup x (toList xys)

filter :: (a->Bool) -> RAList a -> RAList a
filter p xs =
    case uncons xs of
      Nothing -> empty
      Just(h,tl) ->
        let
           ys = filter p tl
        in
           if p h then h `cons` ys else  ys


partition :: (a->Bool) -> RAList a -> (RAList a, RAList a)
partition p xs = (filter p xs, filter (not . p) xs)




zip :: RAList a -> RAList b -> RAList (a, b)
zip = zipWith (,)

zipWith :: forall a b c .  (a->b->c) -> RAList a -> RAList b -> RAList c
zipWith f xs1 xs2 = case compare (wLength xs1) (wLength xs2) of
                      EQ ->  zipTop xs1 xs2
                      LT ->   zipTop  xs1
                                      (take (wLength xs1) xs2)   --  error "xs1 is smaller than xs2"
                      GT ->   zipTop  (take (wLength xs2) xs1)
                                      xs2 --  error "x2 is smaller than xs1"

    --      | s1 == s2 = RAList s1 (zipTop wts1 wts2)
    --    | otherwise = fromList $ Prelude.zipWith f (toList xs1) (toList xs2)
  where zipTree (Leaf x1) (Leaf x2) = Leaf (f x1 x2)
        zipTree (Node x1 l1 r1) (Node x2 l2 r2) = Node (f x1 x2) (zipTree l1 l2) (zipTree r1 r2)
        zipTree _ _ = error "Data.RAList.zipWith: impossible"
        zipTop :: RAList a -> RAList b -> RAList c
        zipTop RNil RNil = RNil
        zipTop (RCons tot1 w t1 xss1) (RCons _tot2 _ t2 xss2) = RCons tot1 w (zipTree t1 t2) (zipTop xss1 xss2)
        zipTop _ _ = error "Data.RAList.zipWith: impossible"

unzip :: RAList (a, b) -> (RAList a, RAList b)
unzip xs = (map fst xs, map snd xs)

-- | Change element at the given index.
-- Complexity /O(log n)/.
update :: Word64 -> a -> RAList a -> RAList a
update i x = adjust (const x) i

-- | Apply a function to the value at the given index.
-- Complexity /O(log n)/.
adjust :: forall a . (a->a) -> Word64 -> RAList a -> RAList a
adjust f n s | n <  0 = error "Data.RAList.adjust: negative index"
                          | n >= (genericLength s) = error "Data.RAList.adjust: index too large"
                          | otherwise = (adj n s )
  where adj  :: Word64 -> RAList a -> RAList a
        adj j (RCons tot  w t wts') | j < w     = RCons tot w (adjt j (w `quot` 2) t) wts'
                              | otherwise = RCons tot w t (adj (j-w) wts')
        adj j _ = error ("Data.RAList.adjust: impossible Nil element: " <> show j)
        adjt 0 0 (Leaf x) = Leaf (f x)
        adjt 0 _ (Node x l r) = Node (f x) l r
        adjt j w (Node x l r) | j <= w    = Node x (adjt (j-1) (w `quot` 2) l) r
                              | otherwise = Node x l (adjt (j-1-w) (w `quot` 2) r)
        adjt _ _ _ = error "Data.RAList.adjust: impossible"

-- XXX Make this a good producer
-- | Complexity /O(n)/.
--toList :: RAList a -> [a]
--toList ra = tops ra []
--  where flat (Leaf x)     a = x : a
--        flat (Node x l r) a = x : flat l (flat r a)
--        tops RNil r = r
--        tops (RCons _tot _ t xs) r = flat t (tops xs r)

-- XXX Use number system properties to make this more efficient.
-- | Complexity /O(n)/.
fromList :: [a] -> RAList a
fromList = Prelude.foldr Cons Nil

errorEmptyList :: String -> a
errorEmptyList fun =
  error ("Data.RAList." Prelude.++ fun Prelude.++ ": empty list")


--- copy fusion codes of your own :) perhaps?
--- for now these fusion rules are shamelessly copied from the ghc base library

{-# INLINE [1] build #-}
--- a
build   :: forall a. (forall b. (a -> b -> b) -> b -> b) -> RAList a
build = \ g -> g  cons Nil

unfoldr :: (b -> Maybe (a, b)) -> b -> RAList a
{-# INLINE unfoldr #-} -- See Note [INLINE unfoldr  in ghc base library original source]
unfoldr f b0 = build (\c n ->
  let go b = case f b of
               Just (a, new_b) -> a `c` go new_b
               Nothing         -> n
  in go b0)


augment :: forall a. (forall b. (a->b->b) -> b -> b) -> RAList a -> RAList a
{-# INLINE [1] augment #-}
augment g xs = g cons xs



{-# RULES
"fold/build"    forall k z (g::forall b. (a->b->b) -> b -> b) .
                foldr k z (build g) = g k z

"foldr/augment" forall k z xs (g::forall b. (a->b->b) -> b -> b) .
                foldr k z (augment g xs) = g k (foldr k z xs)


"augment/build" forall (g::forall b. (a->b->b) -> b -> b)
                       (h::forall b. (a->b->b) -> b -> b) .
                       augment g (build h) = build (\c n -> g c (h c n))
#-}



{-
additional ru

"foldr/id"                        foldr (:) [] = \x  -> x
        -- Only activate this from phase 1, because that's
        -- when we disable the rule that expands (++) into foldr

-- The foldr/cons rule looks nice, but it can give disastrously
-- bloated code when commpiling
--      array (a,b) [(1,2), (2,2), (3,2), ...very long list... ]
-- i.e. when there are very very long literal lists
-- So I've disabled it for now. We could have special cases
-- for short lists, I suppose.
-- "foldr/cons" forall k z x xs. foldr k z (x:xs) = k x (foldr k z xs)

"foldr/single"  forall k z x. foldr k z [x] = k x z
"foldr/nil"     forall k z.   foldr k z []  = z

"foldr/cons/build" forall k z x (g::forall b. (a->b->b) -> b -> b) .
                           foldr k z (x:build g) = k x (g k z)

-}
