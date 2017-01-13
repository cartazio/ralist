{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE KindSignatures #-}
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
     RAList

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
#if !MIN_VERSION_base(4,9,0) == 1
import Data.Monoid(Monoid,mappend,mempty)
#endif
import Data.Semigroup(Semigroup,(<>))
import Data.Data(Data,Typeable)
import Data.Functor.Identity(runIdentity)

infixl 9  !!
infixr 5  `cons`, ++

-- A RAList is stored as a list of trees.  Each tree is a full binary tree.
-- The sizes of the trees are monotonically increasing, except that the two
-- first trees may have the same size.
-- The first few tree sizes:
-- [ [], [1], [1,1], [3], [1,3], [1,1,3], [3,3], [7], [1,7], [1,1,7],
--   [3,7], [1,3,7], [1,1,3,7], [3,3,7], [7,7], [15], ...
-- (I.e., skew binary numbers.)
data RAList a = RAList {-# UNPACK #-} !Int !(Top a)
    deriving (Eq,Data,Typeable,Foldable)

instance (Show a) => Show (RAList a) where
    showsPrec p xs = showParen (p >= 10) $ showString "fromList " . showsPrec 10 (toList xs)

instance (Read a) => Read (RAList a) where
    readsPrec p = readParen (p > 10) $ \ r -> [(fromList xs, t) | ("fromList", s) <- lex r, (xs, t) <- reads s]

instance (Ord a) => Ord (RAList a) where
    xs <  ys        = toList xs <  toList ys
    xs <= ys        = toList xs <= toList ys
    xs >  ys        = toList xs >  toList ys
    xs >= ys        = toList xs >= toList ys
    xs `compare` ys = toList xs `compare` toList ys

instance Monoid (RAList a) where
    mempty  = empty
    mappend = (<>)

instance Semigroup (RAList a) where
    (<>) = (++)

instance Functor RAList where
    fmap f (RAList s wts) = RAList s (fmap f wts)

instance Applicative RAList where
    pure = \x -> RAList 1 (Cons 1 (Leaf x) Nil)
    (<*>) = zipWith ($)

instance Monad RAList where
    return = pure
    (>>=) = flip concatMap

-- Special list type for (Int, Tree a), i.e., Top a ~= [(Int, Tree a)]
data Top a = Nil | Cons {-# UNPACK #-} !Int !(Tree a) (Top a)
    deriving (Eq,Data,Typeable,Functor,Foldable,Traversable)

--instance Functor Top where
--    fmap _ Nil = Nil
--    fmap f (Cons w t xs) = Cons w (fmap f t) (fmap f xs)

-- Complete binary tree.  The completeness of the trees is an invariant that must
-- be preserved for the implementation to work.
data Tree a
     = Leaf a
     | Node a !(Tree a) !(Tree a)
     deriving (Eq,Data,Typeable,Functor,Foldable,Traversable)

--instance Functor Tree where
--     fmap f (Leaf x)     = Leaf (f x)
--     fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

-----

empty :: RAList a
empty = RAList 0 Nil

-- | Complexity /O(1)/.
cons :: a -> RAList a -> RAList a
cons x (RAList s wts) = RAList (s+1) $
    case wts of
    Cons s1 t1 (Cons s2 t2 wts') | s1 == s2 -> Cons (1 + s1 + s2) (Node x t1 t2) wts'
    _ -> Cons 1 (Leaf x) wts

(++) :: RAList a -> RAList a -> RAList a
xs ++ ys | null ys   = xs                   -- small optimization to avoid consing to empty
         | otherwise = foldr cons ys xs


uncons :: RAList a -> Maybe (a, RAList a)
uncons (RAList _ Nil) =  Nothing
uncons (RAList s (Cons _ (Leaf h)     wts)) =  Just (h,RAList (s-1) wts)
uncons (RAList s (Cons w (Node x l r) wts)) = Just (x, RAList (s-1) (Cons w2 l (Cons w2 r wts)))
  where w2 = w `quot` 2

-- | Complexity /O(1)/.
head :: RAList a -> Maybe a
head = fmap fst  . uncons

-- | Complexity /O(log n)/.
last :: RAList a -> a
last xs@(RAList s _) = xs !! (s-1)

half :: Int -> Int
half n = n `quot` 2

-- | Complexity /O(log n)/.
(!!) :: RAList a -> Int -> a
RAList s wts !! n | n <  0 = error "Data.RAList.!!: negative index"
                  | n >= s = error "Data.RAList.!!: index too large"
                  | otherwise = ix n wts
  where ix j (Cons w t wts') | j < w     = ixt j (w `quot` 2) t
                             | otherwise = ix (j-w) wts'
        ix _ _ = error "Data.RAList.!!: impossible"
        ixt 0 0 (Leaf x) = x
        ixt 0 _ (Node x _l _r) = x
        ixt j w (Node _x l r) | j <= w    = ixt (j-1)   (w `quot` 2) l
                             | otherwise = ixt (j-1-w) (w `quot` 2) r
        ixt _j _w _ = error "Data.RAList.!!: impossible"

lookup :: forall a. Int -> Top a -> a
lookup i xs = runIdentity (lookupM i xs)

lookupM :: forall (m :: * -> *) a. Monad m => Int -> Top a -> m a
lookupM jx zs = look zs jx
  where look Nil _ = fail "RandList.lookup bad subscript"
        look (Cons j t xs) i
            | i < j     = lookTree j t i
            | otherwise = look xs (i - j)

        lookTree _ (Leaf x) i
            | i == 0    = return x
            | otherwise = nothing
        lookTree j (Node x s t) i
            | i > k  = lookTree k t (i - 1 - k)
            | i /= 0 = lookTree k s (i - 1)
            | otherwise = return x
          where k = half j
        nothing = fail "RandList.lookup: not found"
        --- this wont fly long term

lookupWithDefault :: forall t. t -> Int -> Top t -> t
lookupWithDefault d jx zs = look zs jx
  where look Nil _ = d
        look (Cons j t xs) i
            | i < j     = lookTree j t i
            | otherwise = look xs (i - j)

        lookTree _ (Leaf x) i
            | i == 0    = x
            | otherwise = d
        lookTree j (Node x s t) i
            | i > k   = lookTree k t (i - 1 - k)
            | i /= 0  = lookTree k s (i - 1)
            | otherwise = x
          where k = half j

-- | Complexity /O(1)/.
tail :: RAList a -> Maybe (RAList a)
tail = fmap snd . uncons
-- XXX Is there some clever way to do this?
init :: RAList a -> RAList a
init = fromList . Prelude.init . toList

null :: RAList a -> Bool
null (RAList s _) = s == 0

-- | Complexity /O(1)/.
length :: RAList a -> Int
length (RAList s _) = s

map :: (a->b) -> RAList a -> RAList b
map = fmap



reverse :: RAList a -> RAList a
reverse = fromList . Prelude.reverse . toList

-- XXX All the folds could be done more effiently.
foldl :: (a -> b -> a) -> a -> RAList b -> a
foldl f z xs = Prelude.foldl f z (toList xs)

foldl' :: (a -> b -> a) -> a -> RAList b -> a
foldl' f z xs = List.foldl' f z (toList xs)

foldl1 :: (a -> a -> a) -> RAList a -> a
foldl1 f xs | null xs = errorEmptyList "foldl1"
            | otherwise = Prelude.foldl1 f (toList xs)

foldl1' :: (a -> a -> a) -> RAList a -> a
foldl1' f xs | null xs = errorEmptyList "foldl1'"
             | otherwise = List.foldl1' f (toList xs)

-- XXX This could be deforested.
foldr :: (a -> b -> b) -> b -> RAList a -> b
foldr f z xs = Prelude.foldr f z (toList xs)

foldr1 :: (a -> a -> a) -> RAList a -> a
foldr1 f xs | null xs = errorEmptyList "foldr1"
            | otherwise = Prelude.foldr1 f (toList xs)

concat :: RAList (RAList a) -> RAList a
concat = foldr (++) empty

concatMap :: (a -> RAList b) -> RAList a -> RAList b
concatMap f = concat . map f

and :: RAList Bool -> Bool
and = foldr (&&) True

or :: RAList Bool -> Bool
or = foldr (||) False

any :: (a -> Bool) -> RAList a -> Bool
any p = or . map p

all :: (a -> Bool) -> RAList a -> Bool
all p = and . map p

sum :: (Num a) => RAList a -> a
sum = foldl (+) 0

product :: (Num a) => RAList a -> a
product = foldl (*) 1

maximum :: (Ord a) => RAList a -> a
maximum xs | null xs   = errorEmptyList "maximum"
           | otherwise = foldl1 max xs

minimum :: (Ord a) => RAList a -> a
minimum xs | null xs   = errorEmptyList "minimum"
           | otherwise = foldl1 min xs

replicate :: Int -> a -> RAList a
replicate n = fromList . Prelude.replicate n

take :: Int -> RAList a -> RAList a
take n = fromList . Prelude.take n . toList



-- | Complexity /O(log n)/.
drop :: Int -> RAList a -> RAList a
drop n xs | n <= 0 = xs
drop n _xs@(RAList s _) | n >= s = empty
drop n (RAList s wts) = RAList (s-n) (loop n wts)
  where loop 0 xs = xs
        loop n1 (Cons w _ xs) | w <= n1 = loop (n1-w) xs -- drops full trees
        loop n2 (Cons w tre xs) = splitTree n2 w tre xs -- splits tree
        loop _ _ = error "Data.RAList.drop: impossible"

-- helper function for drop
-- drops the first n elements of the tree and adds them to the front
splitTree :: Int -> Int -> Tree a -> Top a -> Top a
splitTree n treeSize tre xs | n == 0 = Cons treeSize tre xs
splitTree n treeSize (Node _ l r) xs
  | n == 1 = Cons halfTreeSize l (Cons halfTreeSize r xs)
  | n <= halfTreeSize = splitTree (n-1) halfTreeSize l (Cons halfTreeSize r xs)
  | n > halfTreeSize = splitTree (n-halfTreeSize-1) halfTreeSize r xs
    where halfTreeSize = treeSize `quot` 2
splitTree n treeSize nd xs
  | n == 1 = xs
  | n == 0 = Cons treeSize nd xs


-- Old version of drop
-- worst case complexity /O(n)/
simpleDrop :: Int -> RAList a -> RAList a
simpleDrop n xs | n <= 0 = xs
simpleDrop n _xs@(RAList s _) | n >= s = empty
simpleDrop n (RAList s wts) = RAList (s-n) (loop n wts)
    where loop 0 xs = xs
          loop n1 (Cons w _ xs) | w <= n1 = loop (n1-w) xs
          loop n2 (Cons w (Node _ l r) xs) = loop (n2-1) (Cons w2 l (Cons w2 r xs))
            where w2 = w `quot` 2
          loop _ _ = error "Data.RAList.drop: impossible"


splitAt :: Int -> RAList a -> (RAList a, RAList a)
splitAt n xs = (take n xs, drop n xs)

elem :: (Eq a) => a -> RAList a -> Bool
elem x = any (== x)

notElem :: (Eq a) => a -> RAList a -> Bool
notElem x = not . elem x -- aka all (/=)

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

zipWith :: (a->b->c) -> RAList a -> RAList b -> RAList c
zipWith f xs1@(RAList s1 wts1) xs2@(RAList s2 wts2)
    | s1 == s2 = RAList s1 (zipTop wts1 wts2)
    | otherwise = fromList $ Prelude.zipWith f (toList xs1) (toList xs2)
  where zipTree (Leaf x1) (Leaf x2) = Leaf (f x1 x2)
        zipTree (Node x1 l1 r1) (Node x2 l2 r2) = Node (f x1 x2) (zipTree l1 l2) (zipTree r1 r2)
        zipTree _ _ = error "Data.RAList.zipWith: impossible"
        zipTop Nil Nil = Nil
        zipTop (Cons w t1 xss1) (Cons _ t2 xss2) = Cons w (zipTree t1 t2) (zipTop xss1 xss2)
        zipTop _ _ = error "Data.RAList.zipWith: impossible"

unzip :: RAList (a, b) -> (RAList a, RAList b)
unzip xs = (map fst xs, map snd xs)

-- | Change element at the given index.
-- Complexity /O(log n)/.
update :: Int -> a -> RAList a -> RAList a
update i x = adjust (const x) i

-- | Apply a function to the value at the given index.
-- Complexity /O(log n)/.
adjust :: (a->a) -> Int -> RAList a -> RAList a
adjust f n (RAList s wts) | n <  0 = error "Data.RAList.adjust: negative index"
                          | n >= s = error "Data.RAList.adjust: index too large"
                          | otherwise = RAList s (adj n wts)
  where adj j (Cons w t wts') | j < w     = Cons w (adjt j (w `quot` 2) t) wts'
                              | otherwise = Cons w t (adj (j-w) wts')
        adj j _ = error ("Data.RAList.adjust: impossible Nil element: " <> show j)
        adjt 0 0 (Leaf x) = Leaf (f x)
        adjt 0 _ (Node x l r) = Node (f x) l r
        adjt j w (Node x l r) | j <= w    = Node x (adjt (j-1) (w `quot` 2) l) r
                              | otherwise = Node x l (adjt (j-1-w) (w `quot` 2) r)
        adjt _ _ _ = error "Data.RAList.adjust: impossible"

-- XXX Make this a good producer
-- | Complexity /O(n)/.
toList :: RAList a -> [a]
toList (RAList _ wts) = tops wts []
  where flat (Leaf x)     a = x : a
        flat (Node x l r) a = x : flat l (flat r a)
        tops Nil r = r
        tops (Cons _ t xs) r = flat t (tops xs r)

-- XXX Use number system properties to make this more efficient.
-- | Complexity /O(n)/.
fromList :: [a] -> RAList a
fromList = Prelude.foldr cons empty

errorEmptyList :: String -> a
errorEmptyList fun =
  error ("Data.RAList." Prelude.++ fun Prelude.++ ": empty list")
