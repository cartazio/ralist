module Main where

import Data.RAList
import Test.Hspec
import Control.Exception (evaluate)

import Prelude hiding (
    (++), head, last, tail, init, null, length, map, reverse,
    foldl, foldl1, foldr, foldr1, concat, concatMap,
    and, or, any, all, sum, product, maximum, minimum, take,
    drop, elem, splitAt, notElem, lookup, replicate, (!!), filter,
    zip, zipWith, unzip
    )
import qualified Prelude

empty :: RAList a
empty = Nil

main = hspec $ do
  describe "RAList.cons" $ do
    it "adds to an empty list" $ do
      cons 1 empty `shouldBe` (fromList [1] :: RAList Int)
    it "adds to a list of length 1" $ do
      cons 1 (fromList [2]) `shouldBe` (fromList [1,2] :: RAList Int)
    it "add to a list of length 10" $ do
      cons 1 (fromList [2..11]) `shouldBe`
        (fromList [1..11] :: RAList Int)


  describe "RAList.uncons" $ do
    it "deletes the first element of a list of length 1" $ do
      uncons (fromList ['a']) `shouldBe`
        (Just ('a',(fromList [])) :: Maybe (Char, RAList Char))
    it "deletes the first element of a list of length 3" $ do
      uncons (fromList ['a'..'c']) `shouldBe`
        (Just ('a',(fromList ['b','c'])) :: Maybe (Char, RAList Char))
    it "returns Nothing when it uncons from an empty list" $ do
      (uncons empty :: Maybe (Char, RAList Char)) `shouldBe`
        (Nothing :: Maybe (Char, RAList Char))

  describe "RAList.head" $ do -- This is a Maybe
    it "gets the first element of a list of length 1" $ do
      (Data.RAList.head (fromList [1 :: Int ]) ) `shouldBe` Just 1
    it "gets the first element of a list of length 3" $ do
      (Data.RAList.head (fromList [1 .. 3 :: Int ])) `shouldBe` Just 1
    it "gets the first element of a list of length 9" $ do
      (Data.RAList.head (fromList [1 .. 9 :: Int])) `shouldBe` Just 1
    it "gets nothing if the list is empty" $ do
      Data.RAList.head (fromList ([] :: [Int])) `shouldBe` Nothing

  describe "RAList.last" $ do -- This is not a Maybe
    it "gets the last element of a list of length 1" $ do
       (Data.RAList.last (fromList [1]) ) `shouldBe`
        (1 :: Integer)
    it "gets the last element of a list of length 3" $ do
      Data.RAList.last (fromList [1..3]) `shouldBe`
        (3 :: Integer)
    it "gets the last element of a list of length 9" $ do
      Data.RAList.last (fromList [1..9]) `shouldBe`
        (9 :: Integer)
    it "gets nothing if the list is empty" $ do
      evaluate (Data.RAList.last empty) `shouldThrow` anyException

  describe "RAList.tail" $ do -- This is a Maybe
    it "gets everything after the first element of a list of length 1" $ do
      (Data.RAList.tail (fromList [1]) ) `shouldBe`
        (Just (fromList []) :: Maybe (RAList Integer))
    it "gets everything after the first element of a list of length 3" $ do
      (Data.RAList.tail (fromList [1..3])) `shouldBe`
        (Just (fromList [2,3]) :: Maybe (RAList Integer))
    it "gets everything after the first element of a list of length 9" $ do
      (Data.RAList.tail (fromList [1..9])) `shouldBe`
        (Just (fromList [2..9]) :: Maybe (RAList Integer))
    it "gets nothing if the list is empty" $ do
      Data.RAList.tail empty `shouldBe` (Nothing :: Maybe (RAList Integer))

  describe "RAList.init" $ do -- This is not a Maybe
    it "gets everything before the last element of a list of length 1" $ do
       (Data.RAList.init (fromList [1]) ) `shouldBe` (fromList [] :: RAList Integer)
    it "gets everything but the last element of a list of length 3" $ do
      Data.RAList.init (fromList [1..3]) `shouldBe` (fromList [1,2] :: RAList Integer)
    it "gets everything but the last element of a list of length 9" $ do
      Data.RAList.init (fromList [1..9]) `shouldBe` (fromList [1..8] :: RAList Integer)

    it "gets nothing if the list is empty" $ do
      evaluate (Data.RAList.init empty) `shouldThrow` anyException

  describe "RAList.null" $ do
    it "returns True if it is given an empty list" $ do
      Data.RAList.null empty `shouldBe` (True :: Bool)
    it "return False if it is given a non-empty list" $ do
      Data.RAList.null (fromList [1]) `shouldBe` (False :: Bool)

  describe "RAList.length" $ do
    it "returns 0 if the list is empty" $ do
      Data.RAList.length empty `shouldBe` 0
    it "returns 1 if the list has length 1" $ do
      Data.RAList.length (fromList [1]) `shouldBe` 1
    it "returns 3 if the list has lenght 3"$ do
      Data.RAList.length (fromList [1..3]) `shouldBe` 3
    it "returns 9 if the list has length 9" $ do
      Data.RAList.length (fromList [1..9]) `shouldBe`  9

  describe "Ralist.(!!)" $ do
    it "!! 0"  $ do
      (fromList [ 1 .. 9]) !! 0 `shouldBe` 1
    it "!! 1"  $ do
      (fromList [ 1 .. 9]) !! 1 `shouldBe` 2
    it "!! 2"  $ do
      (fromList [ 1 .. 9]) !! 2 `shouldBe` 3
    it "!! 3"  $ do
      (fromList [ 1 .. 9]) !! 3 `shouldBe` 4
    it "!! 4"  $ do
      (fromList [ 1 .. 9]) !! 4 `shouldBe` 5
    it "!! 5"  $ do
      (fromList [ 1 .. 9]) !! 5 `shouldBe` 6
    it "!! 6"  $ do
      (fromList [ 1 .. 9]) !! 6 `shouldBe` 7
    it "!! 7"  $ do
      (fromList [ 1 .. 9]) !! 7 `shouldBe` 8
    it "!! 8"  $ do
      (fromList [ 1 .. 9]) !! 8 `shouldBe` 9


  describe "RAList.lookupL" $ do
    describe "for a list of length 1" $ do
      let ra = fromList [('a','b')]
      it "returns the first value correctly" $ do
        lookupL 'a' ra `shouldBe` (Just 'b' :: Maybe Char)
      it "returns Nothing for a nonexistent key value" $ do
        lookupL 'z' ra `shouldBe` (Nothing :: Maybe Char)
    describe "for a list of length 3" $ do
      let ra = fromList [('a','b'),('c','d'),('e','f')]
      it "returns the first value correctly" $ do
        lookupL 'a' ra `shouldBe` (Just 'b':: Maybe Char)
      it "returns the last value correctly" $ do
        lookupL 'e' ra `shouldBe` (Just 'f':: Maybe Char)
      it "returns the middle value correctly" $ do
        lookupL 'c' ra `shouldBe` (Just 'd':: Maybe Char)
      it "returns Nothing for a nonexistent key value" $ do
        lookupL 'z' ra `shouldBe` (Nothing :: Maybe Char)
    describe "for a list of length 9" $ do
      let ra = fromList [('a','b'),('c','d'),('e','f'),('g','h'),('i','j'),('k','l'),('m','n'),('o','p'),('q','r')]
      it "returns the first value correctly" $ do
        lookupL 'a' ra `shouldBe` (Just 'b':: Maybe Char)
      it "returns the second value correctly" $ do
        lookupL 'c' ra `shouldBe` (Just 'd':: Maybe Char)
      it "returns the third value correctly" $ do
        lookupL 'e' ra `shouldBe` (Just 'f':: Maybe Char)
      it "returns the fourth value correctly" $ do
        lookupL 'g' ra `shouldBe` (Just 'h':: Maybe Char)
      it "returns the last value correctly" $ do
        lookupL 'q' ra `shouldBe` (Just 'r':: Maybe Char)
      it "returns Nothing for a nonexistent key value" $ do
        lookupL 'z' ra `shouldBe` (Nothing :: Maybe Char)
    describe "for an empty list" $ do
      it "return Nothing when called with an empty list" $ do
        lookupL 'a' empty `shouldBe` (Nothing :: Maybe Char)

  describe "RAList.map" $ do
    describe "for a list of length 1" $ do
      it "maps from Int to Int"$ do
        Data.RAList.map (\x -> 2*x) (fromList [1])  `shouldBe`
          (fromList [2] :: RAList Int)
      it "maps from Int to String" $ do
        Data.RAList.map (\x -> 'a') (fromList [1]) `shouldBe`
          (fromList ['a'] :: RAList Char)
      it "maps from Char to Int" $ do
        Data.RAList.map (\x -> 1) (fromList ['a']) `shouldBe`
          (fromList [1] :: RAList Int)
      it "maps from Int to [Int]" $ do
        Data.RAList.map (\x -> [x]) (fromList [1]) `shouldBe`
          (fromList [[1]] :: RAList [Int])
      it "maps from [Int] to Int" $ do
        Data.RAList.map (\x -> x Prelude.!! 0)
          ((fromList [[1]]) :: RAList [Int])
          `shouldBe` (fromList [1] :: RAList Int)
    describe "for a list of length 3" $ do
      it "maps from Int to Int" $ do
        Data.RAList.map (\x -> 2*x) (fromList [1..3]) `shouldBe`
          (fromList [2,4,6] :: RAList Int)
    describe "for a list of length 9" $ do
      it "maps from Int to Int" $ do
        Data.RAList.map (\x -> 2*x) (fromList [1..9])
          `shouldBe`
          ((fromList [2,4..18]) :: RAList Int)
    describe "for an empty list" $ do
      it "returns an empty list of correct type" $ do
        Data.RAList.map (\x -> 'a') empty `shouldBe`
          (empty :: RAList Char)

  describe "RAList.reverse" $ do
    it "does nothing to a list of length 1" $ do
      Data.RAList.reverse (fromList [1]) `shouldBe`
        (fromList [1] :: RAList Int)
    it "reverses a list of length 3" $ do
      Data.RAList.reverse (fromList [1..3]) `shouldBe`
        (fromList [3,2,1] :: RAList Int)
    it "reverse a list of length 9" $ do
      Data.RAList.reverse (fromList [1..9]) `shouldBe`
        (fromList [9,8..1] :: RAList Int)
    it "does nothing to an empty list" $ do
      Data.RAList.reverse (empty :: RAList Int) `shouldBe`
        (empty :: RAList Int)

  -- Only tests for one list length because it immediately turns the RALists
  -- into standard lists with 'toList', which is tested elsewhere
  describe "folds" $ do
    let rai = fromList [1..3]
    let ras = fromList ["a","b","c"]
    describe "RAList.foldl" $ do
      it "adds Ints in a list" $ do
        Data.RAList.foldl (+) 0 rai `shouldBe` (6 :: Int)
      it "subtracts Ints in a list" $ do
        Data.RAList.foldl (-) 0 rai `shouldBe` ((-6) :: Int)
      it "concatenates Strings in a list" $ do
        Data.RAList.foldl (Prelude.++) "" ras `shouldBe`
          ("abc" :: [Char])
    describe "RAList.foldl1" $  do
      it "adds Ints in a list" $ do
        Data.RAList.foldl1 (+) rai `shouldBe` (6 :: Int)
      it "subtracts Ints in a list" $ do
        Data.RAList.foldl1 (-) rai `shouldBe` ((-4) :: Int)
      it "concatenates Strings in a list" $ do
        Data.RAList.foldl1 (Prelude.++) ras `shouldBe`
          ("abc" :: [Char])
    describe "RAList.foldr" $ do
      it "adds Ints in a list" $ do
        Data.RAList.foldr (+) 0 rai `shouldBe` (6 :: Int)
      it "subtracts Ints in a list" $ do
        Data.RAList.foldr (-) 0 rai `shouldBe` (2 :: Int)
      it "concatenates Strings in a list" $ do
        Data.RAList.foldr (Prelude.++) "" ras `shouldBe`
          ("abc" :: [Char])
    describe "RAList.foldr1" $ do
      it "adds Ints in a list" $ do
        Data.RAList.foldr1 (+) rai `shouldBe` (6 :: Int)
      it "subtracts Ints in a list" $ do
        Data.RAList.foldr1 (-) rai `shouldBe` (2 :: Int)
      it "concatenates characters in a list" $ do
        Data.RAList.foldr1 (Prelude.++) ras `shouldBe`
          ("abc" :: [Char])

  describe "RAList.concat" $ do
    it "concatenates list of empty lists" $ do
      Data.RAList.concat (fromList [empty, empty]) `shouldBe`
        (empty :: RAList Integer)
    it "concatenates an empty list" $ do
      Data.RAList.concat empty `shouldBe` (empty :: RAList Integer)
    it "concatenates lists of the same length" $ do
      Data.RAList.concat
        (fromList [fromList [1,2], fromList [1,2], fromList [1,2]])
        `shouldBe` (fromList [1,2,1,2,1,2] :: RAList Int)
    it "concatenates lists of different lengths" $ do
      Data.RAList.concat
        (fromList [fromList [], fromList [1], fromList [1,2],
          fromList [1..3], fromList [1..9]])
        `shouldBe`
        (fromList [1,1,2,1,2,3,1,2,3,4,5,6,7,8,9] :: RAList Int)

  describe "RAList.concatMap" $ do
    describe "for lists of length 1" $ do
       it "maps (x -> [x]) on a list of integers" $ do
         Data.RAList.concatMap (\x -> fromList [x])
           (fromList [1]) `shouldBe`
           (fromList [1] :: RAList Int)
       it "maps (x -> [1, length x]) on a list of Strings" $ do
         Data.RAList.concatMap
           (\x -> fromList [1, (Prelude.length x)])
           (fromList ["abc"]) `shouldBe`
           (fromList [1,3] :: RAList Int)
       it "returns an empty list when it maps to an empty list" $ do
         Data.RAList.concatMap
           (\x -> empty) (fromList [1]) `shouldBe`
           (fromList [] :: RAList Int)
    describe "for lists of length 3" $ do
      it "maps (x -> [x]) on a list of integers" $ do
        Data.RAList.concatMap (\x -> fromList [x])
          (fromList [1..3]) `shouldBe`
          (fromList [1..3] :: RAList Int)
      it "maps (x -> [1, length x]) on a list of Strings" $ do
        Data.RAList.concatMap
          (\x -> fromList [1, (Prelude.length x)])
          (fromList ["a","ab", "abc"]) `shouldBe`
          (fromList [1,1,1,2,1,3] :: RAList Int)
      it "returns an empty list when it maps to an empty list" $ do
        Data.RAList.concatMap
          (\x -> empty) (fromList [1..3]) `shouldBe`
          (fromList [] :: RAList Int)
    describe "for lists of length " $ do
        it "maps (x -> [x]) on a list of integers" $ do
          Data.RAList.concatMap (\x -> fromList [x])
            (fromList [1..9]) `shouldBe`
            (fromList [1..9] :: RAList Int)
        it "maps (x -> [1, length x]) on a list of Strings" $ do
          Data.RAList.concatMap
            (\x -> fromList [1, (Prelude.length x)])
            (fromList ["a","ab", "abc","a","a","a","a","a","abcd"])
            `shouldBe`
            (fromList [1,1,1,2,1,3,1,1,1,1,1,1,1,1,1,1,1,4]
            :: RAList Int)
        it "returns an empty list when it maps to an empty list" $ do
          Data.RAList.concatMap
            (\x -> empty) (fromList [1..9]) `shouldBe`
            (fromList [] :: RAList Int)

  describe "logic functions" $ do
    describe "Data.RAList.and" $ do
      it "returns False when first value is False" $ do
        Data.RAList.and
          (fromList [False, True, True, True, True, True, True, True,
          True])
          `shouldBe` (False :: Bool)
      it "returns False when last value is True" $ do
        Data.RAList.and
           (fromList [True, True, True, True, True, True, True,
          True, False])
          `shouldBe` (False :: Bool)
      it "returns False when the only value is False" $ do
        Data.RAList.and
          (fromList [False, False, False, False, False, False, False,
          False, False])
          `shouldBe` (False :: Bool)
      it "returns False with a list of length one containing False" $ do
        Data.RAList.and (fromList [False]) `shouldBe` (False :: Bool)
      it " returns True with a list of length one containing True" $ do
        Data.RAList.and (fromList [True]) `shouldBe` (True :: Bool)
      it "returns True when the only value is True" $ do
        Data.RAList.and
          (fromList [True, True, True, True, True, True, True, True,
          True])
          `shouldBe` (True :: Bool)
      it "returns True for an empty list" $ do
        Data.RAList.and empty `shouldBe` (True :: Bool)
    describe "Ralist.or" $ do
      it "returns True when first value is True" $ do
        Data.RAList.or
          (fromList [True, False, False, False, False, False, False,
          False, False])
          `shouldBe` (True :: Bool)
      it "returns True when last value is True" $ do
        Data.RAList.or
          (fromList [False, False, False, False, False, False, False,
          False, True])
        `shouldBe` (True :: Bool)
      it "returns False when the only value is False" $ do
        Data.RAList.or
          (fromList [False, False, False, False, False, False, False,
          False, False])
          `shouldBe` (False :: Bool)
      it "returns False with a list of length one containing False" $ do
        Data.RAList.or (fromList [False]) `shouldBe` (False :: Bool)
      it " returns True with a list of length one containing True" $ do
        Data.RAList.or (fromList [True]) `shouldBe` (True :: Bool)
      it "returns True when the only value is True" $ do
        Data.RAList.or
          (Data.RAList.replicate 9 True)
          `shouldBe` (True :: Bool)
      it "returns False for an empty list" $ do
        Data.RAList.or empty `shouldBe` (False :: Bool)
    describe "RAList.any" $ do
      it "returns True when first value evaluates to True" $ do
        Data.RAList.any (\x -> x)
          (fromList [True, False, False, False, False, False, False,
          False, False])
          `shouldBe` (True :: Bool)
      it "returns True when last value is True" $ do
        Data.RAList.any  (\x -> x)
          (fromList [False, False, False, False, False, False, False,
          False, True])
        `shouldBe` (True :: Bool)
      it "returns False when the only value is False" $ do
        Data.RAList.any  (\x -> x)
          (Data.RAList.replicate 9 False)
          `shouldBe` (False :: Bool)
      it "returns False with a list of length one containing False" $ do
        Data.RAList.any (\x -> x) (fromList [False]) `shouldBe`
          (False :: Bool)
      it " returns True with a list of length one containing True" $ do
        Data.RAList.any  (\x -> x) (fromList [True]) `shouldBe` (True :: Bool)
      it "returns True when the only value is True" $ do
        Data.RAList.any  (\x -> x)
          (Data.RAList.replicate 9 True)
          `shouldBe` (True :: Bool)
      it "returns False for an empty list" $ do
        Data.RAList.any (\x -> x) empty `shouldBe` (False :: Bool)
    describe "RAList.all" $ do
       it "returns False when first value evaluates to False" $ do
         Data.RAList.all (\x -> x)
           (fromList [False, True, True, True, True, True, True, True,
           True])
           `shouldBe` (False :: Bool)
       it "returns False when last value evaluates to False" $ do
         Data.RAList.all (\x -> x)
            (fromList [True, True, True, True, True, True, True,
           True, False])
           `shouldBe` (False :: Bool)
       it "returns False when the only value evaluates to False" $ do
         Data.RAList.all (\x -> x)
          (Data.RAList.replicate 9 False)
           `shouldBe` (False :: Bool)
       it "returns False with a list of one value which evaluates to False"
        $ do
         Data.RAList.all (\x -> x) (fromList [False]) `shouldBe` (False :: Bool)
       it " returns True with a list of one value which evaluates to True"
        $ do
         Data.RAList.all (\x -> x) (fromList [True]) `shouldBe` (True :: Bool)
       it "returns True when the only value evaluates to True" $ do
         Data.RAList.all (\x -> x)
           (Data.RAList.replicate 9 True)
           `shouldBe` (True :: Bool)
       it "returns True for an empty list" $ do
         Data.RAList.all (\x -> x) empty `shouldBe` (True :: Bool)

  describe "Math operations" $ do
    describe "RAList.sum" $ do
      it "adds all ints in a list of length 1" $ do
        Data.RAList.sum (fromList [3]) `shouldBe` (3 :: Int)
      it "adds all ints in a list of length 3" $ do
        Data.RAList.sum (fromList [1,1,1]) `shouldBe` (3 :: Int)
      it "adds all ints in a list of length 9" $ do
        Data.RAList.sum (Data.RAList.replicate 9 1) `shouldBe` (9 :: Int)
      it "returns 0 when given an empty list" $ do
        Data.RAList.sum empty `shouldBe` (0 :: Int)
    describe "RAList.product" $ do
      it "multiplies all ints in a list of length 1" $ do
        Data.RAList.product (fromList [3]) `shouldBe` (3 :: Int)
      it "multiplies all ints in a list of length 3" $ do
        Data.RAList.product (fromList [2,2,2]) `shouldBe` (8 :: Int)
      it "multiplies all ints in a list of length 9" $ do
        Data.RAList.product (Data.RAList.replicate 9 2) `shouldBe`
          (512 :: Int)
      it "returns 1 when given an empty list" $ do
        Data.RAList.product empty `shouldBe` (1 :: Int)

  describe "extrema" $ do
    describe "RAList.maximum" $ do
      describe "for list of length 1" $ do
        it "returns the value" $ do
          Data.RAList.maximum (fromList [1]) `shouldBe` (1 :: Int)
      describe "for a list of length 3" $ do
        it "can return the first value" $ do
          Data.RAList.maximum (fromList [3,1,2]) `shouldBe` (3 :: Int)
        it "can return the last value" $ do
          Data.RAList.maximum (fromList [1..3]) `shouldBe` (3 :: Int)
      describe "for a list of length 9" $ do
        let ra = fromList [1..9]
        it "can return the first value" $ do
          Data.RAList.maximum (update 0 10 ra) `shouldBe` (10 :: Int)
        it "can return the second value" $ do
          Data.RAList.maximum (update 1 10 ra) `shouldBe` (10 :: Int)
        it "can return the third value" $ do
          Data.RAList.maximum (update 2 10 ra) `shouldBe` (10 :: Int)
        it "can return the fourth value" $ do
          Data.RAList.maximum (update 3 10 ra) `shouldBe` (10 :: Int)
        it "can return the last value" $ do
          -- Specifies the list rather than updating the last value
          -- because update is currently broken
          Data.RAList.maximum (fromList [1,2,3,4,5,6,7,8,10]) `shouldBe`
            (10 :: Int)
      describe "for an empty list" $ do
        it "throws an exception" $ do
          evaluate (Data.RAList.maximum (empty :: RAList Int)) `shouldThrow`
            anyException
  describe "RAList.minimum" $ do
    describe "other" $ do
      describe "for list of length 1" $ do
        it "returns the value" $ do
          Data.RAList.minimum (fromList [1]) `shouldBe` (1 :: Int)
      describe "for a list of length 3" $ do
        it "can return the first value" $ do
          Data.RAList.minimum (fromList [1,3,2]) `shouldBe` (1 :: Int)
        it "can return the last value" $ do
          Data.RAList.minimum (fromList [3,2,1]) `shouldBe` (1 :: Int)

      describe "for a list of length 9" $ do
        let ra = fromList [1..9]
        it "can return the first value" $ do
          Data.RAList.minimum (update 0 0 ra) `shouldBe` (0 :: Int)
        it "can return the second value" $ do
          Data.RAList.minimum (update 1 0 ra) `shouldBe` (0 :: Int)
        it "can return the third value" $ do
          Data.RAList.minimum (update 2 0 ra) `shouldBe` (0 :: Int)
        it "can return the fourth value" $ do
          Data.RAList.minimum (update 3 0 ra) `shouldBe` (0 :: Int)
        it "can return the last value" $ do
          Data.RAList.minimum (fromList [1,2,3,4,5,6,7,8,0]) `shouldBe`
            (0 :: Int)
      describe "for an empty list" $ do
        it "throws an exception" $ do
          evaluate (Data.RAList.minimum (empty :: RAList Int)) `shouldThrow`
            anyException

  describe "RAList.replicate" $ do
    it "creates an empty list" $ do
      Data.RAList.replicate 0 1 `shouldBe` (empty :: RAList Int)
    it "creates a list of length 1" $ do
      Data.RAList.replicate 1 1 `shouldBe` (fromList [1] :: RAList Int)
    it "creates a list of length 3" $ do
      Data.RAList.replicate 3 1 `shouldBe` (fromList [1,1,1] :: RAList Int)
    it "creates a list of length 9" $ do
      Data.RAList.replicate 9 1 `shouldBe`
        (fromList [1,1,1,1,1,1,1,1,1] :: RAList Int)

  describe "RAList.take" $ do
    let ra = fromList [1..9]
    it "takes the first Data.RAList.element of a list" $ do
      Data.RAList.take  1 ra `shouldBe` (fromList [1] :: RAList Int)
    it "takes the first 2 Data.RAList.elements of a list" $ do
      Data.RAList.take 2 ra `shouldBe` (fromList [1,2] :: RAList Int)
    it "takes the first 3 Data.RAList.elements of a list" $ do
      Data.RAList.take 3 ra `shouldBe` (fromList [1..3] :: RAList Int)
    it "takes the first 4 Data.RAList.elements of a list" $ do
      Data.RAList.take 4 ra `shouldBe` (fromList [1..4] :: RAList Int)
    it "takes all the Data.RAList.elements of a list" $ do
      Data.RAList.take 9 ra `shouldBe` (ra :: RAList Int)
    it "gives an empty list when you Data.RAList.take from an empty list" $ do
      Data.RAList.take 1 empty `shouldBe` (empty :: RAList Int)

  describe "RAList.drop" $ do
    let ra = fromList [1..9]
    it "drops the first Data.RAList.element of a list" $ do
      Data.RAList.drop  1 ra `shouldBe`
        (fromList [2..9] :: RAList Int)
    it "drops the first 2 Data.RAList.elements of a list" $ do
      Data.RAList.drop 2 ra `shouldBe`
        (fromList [3..9] :: RAList Int)
    it "drops the first 3 Data.RAList.elements of a list" $ do
      Data.RAList.drop 3 ra `shouldBe` (fromList [4..9] :: RAList Int)
    it "drops the first 4 Data.RAList.elements of a list" $ do
      Data.RAList.drop 4 ra `shouldBe` (fromList [5..9] :: RAList Int)
    it "drops all the Data.RAList.elements of a list" $ do
      Data.RAList.drop 9 ra `shouldBe` (empty :: RAList Int)
    it "gives an empty list when you drop from an empty list" $ do
      Data.RAList.drop 1 empty `shouldBe` (empty :: RAList Int)

  describe "RAList.splitAt" $ do
    let ra = fromList [1..9]
    it "splitAts the first Data.RAList.element of a list" $ do
      Data.RAList.splitAt  1 ra `shouldBe`
        ((fromList [1], fromList [2..9]) ::
        (RAList Int, RAList Int) )
    it "splitAts the second Data.RAList.element of a list" $ do
      Data.RAList.splitAt 2 ra `shouldBe`
        ((fromList [1,2], fromList [3..9]) ::
        (RAList Int, RAList Int) )
    it "splitAts the third Data.RAList.element of a list" $ do
      Data.RAList.splitAt 3 ra `shouldBe`
        ((fromList [1..3], fromList [4..9]) ::
        (RAList Int, RAList Int) )
    it "splitAts the fourth Data.RAList.element of a list" $ do
      Data.RAList.splitAt 4 ra `shouldBe`
        ((fromList [1..4], fromList [5..9]) ::
        (RAList Int, RAList Int) )
    it "splitAts the last Data.RAList.element of a list" $ do
      Data.RAList.splitAt 9 ra `shouldBe`
        ((ra, empty) :: (RAList Int, RAList Int) )
    it "gives an empty list when you splitAt from an empty list" $ do
      Data.RAList.splitAt 1 empty `shouldBe`
        ((empty, empty) :: (RAList Int, RAList Int) )

  describe "RAList.elem" $ do
    let ra = fromList [1..9]
    it "can find the first Data.RAList.element of a list" $ do
      Data.RAList.elem 1 ra `shouldBe` (True :: Bool)
    it "can find the second Data.RAList.element of a list" $ do
      Data.RAList.elem 2 ra `shouldBe` (True :: Bool)
    it "can find the third Data.RAList.element of a list" $ do
      Data.RAList.elem 3 ra `shouldBe` (True :: Bool)
    it "can find the fourth Data.RAList.element of a list" $ do
      Data.RAList.elem 4 ra `shouldBe` (True :: Bool)
    it "can find the last Data.RAList.element of a list" $ do
      Data.RAList.elem 5 ra `shouldBe` (True :: Bool)
    it "returns false if the Data.RAList.element is not present" $ do
      Data.RAList.elem 10 ra `shouldBe` (False :: Bool)
    it "returns false when given an empty list" $ do
      Data.RAList.elem 1 empty `shouldBe` (False :: Bool)

  describe "RAList.notElem" $ do
    let ra = fromList [1..9]
    it "can find the first Data.RAList.notElement of a list" $ do
      Data.RAList.notElem 1 ra `shouldBe` (False :: Bool)
    it "can find the second Data.RAList.notElement of a list" $ do
      Data.RAList.notElem 2 ra `shouldBe` (False :: Bool)
    it "can find the third Data.RAList.notElement of a list" $ do
      Data.RAList.notElem 3 ra `shouldBe` (False :: Bool)
    it "can find the fourth Data.RAList.notElement of a list" $ do
      Data.RAList.notElem 4 ra `shouldBe` (False :: Bool)
    it "can find the last Data.RAList.notElement of a list" $ do
      Data.RAList.notElem 5 ra `shouldBe` (False :: Bool)
    it "returns false if the Data.RAList.notElement is not present" $ do
      Data.RAList.notElem 10 ra `shouldBe` (True :: Bool)
    it "returns true when give an empty list" $ do
      Data.RAList.notElem 1 empty `shouldBe` (True :: Bool)

  describe "RAList.filter" $ do
    let ra = fromList [1..9]
    it "filters for the first element in a list" $ do
      Data.RAList.filter (\x -> x == 1) ra `shouldBe`
        (fromList [1] :: RAList Int)
    it "filters out the first element in a list" $ do
      Data.RAList.filter (\x -> x > 1) ra `shouldBe`
        (fromList [2..9] :: RAList Int)
    it "filters for the last element in a list" $ do
      Data.RAList.filter (\x -> x == 9) ra `shouldBe`
        (fromList [9] :: RAList Int)
    it "filters out the last element in a list" $ do
      Data.RAList.filter (\x -> x < 9) ra `shouldBe`
        (fromList [1..8] :: RAList Int)
    it "filters for alternating values" $ do
      Data.RAList.filter (\x -> x == 1) (fromList [1,2,1,2,1,2,1,2,1])
        `shouldBe` (fromList [1,1,1,1,1] :: RAList Int)
    it "filters out every value" $ do
      Data.RAList.filter (\x -> False) ra `shouldBe` (empty :: RAList Int)
    it "returns an empty list when given an empty list" $ do
      Data.RAList.filter (\x -> True) empty `shouldBe` (empty :: RAList Int)

  describe "RAList.partition" $ do
    let ra = fromList [1..9]
    it "partitions for the first element in a list" $ do
      Data.RAList.partition (\x -> x == 1) ra `shouldBe`
        ((fromList [1], fromList [2..9]) :: (RAList Int, RAList Int))
    it "partitions out the first element in a list" $ do
      Data.RAList.partition (\x -> x > 1) ra `shouldBe`
        ((fromList [2..9], fromList [1]) :: (RAList Int, RAList Int))
    it "partitions for the last element in a list" $ do
      Data.RAList.partition (\x -> x == 9) ra `shouldBe`
        ((fromList [9], fromList [1..8]) :: (RAList Int, RAList Int))
    it "partitions out the last element in a list" $ do
      Data.RAList.partition (\x -> x < 9) ra `shouldBe`
        ((fromList [1..8], fromList [9]) :: (RAList Int, RAList Int))
    it "partitions for alternating values" $ do
      Data.RAList.partition (\x -> x == 1) (fromList [1,2,1,2,1,2,1,2,1])
        `shouldBe` ((fromList [1,1,1,1,1], fromList [2,2,2,2]) ::
          (RAList Int, RAList Int))
    it "partitions out every value" $ do
      Data.RAList.partition (\x -> False) ra `shouldBe`
        ((empty, ra) :: (RAList Int, RAList Int))
    it "returns an empty list when given an empty list" $ do
      Data.RAList.partition (\x -> True) empty `shouldBe`
        ((empty, empty) :: (RAList Int, RAList Int))

  describe "zip functions" $ do
    let ra1 = (fromList [1..9])
    let ra2 = (fromList ['a','b','c','d','e','f','g','h','i'])
    let ra3 = (fromList ([(1,'a'), (2,'b'), (3,'c'),(4,'d'),(5,'e'),(6,'f'),(7,'g'),(8,'h'),(9,'i')]))
    describe "RAList.zip" $ do
      it "can zip two lists of length 9" $ do
        Data.RAList.zip ra1 ra2 `shouldBe` (ra3 :: RAList (Int, Char))
      it "can zip two lists of length 3" $ do
        Data.RAList.zip (Data.RAList.take 3 ra1) (Data.RAList.take 3 ra2)
        `shouldBe`
          ((Data.RAList.take 3 ra3) :: RAList (Int, Char))
      it "can zip two lists of length 1" $ do
        Data.RAList.zip (Data.RAList.take 1 ra1) (Data.RAList.take 2 ra2)
          `shouldBe`
          (Data.RAList.take 1 ra3 :: RAList (Int,Char))
      it "can zip two lists of different lengths" $ do
        Data.RAList.zip ra1 (Data.RAList.take 5 ra2) `shouldBe`
          (Data.RAList.take 5 ra3 :: RAList (Int, Char))
      it "can zip a list to an empty list" $ do
        Data.RAList.zip empty ra2 `shouldBe`  (empty :: RAList (Int, Char))
      it "can zip two empty lists" $ do
        Data.RAList.zip empty empty `shouldBe` (empty :: RAList (Int, Char))
    describe "RAList.unzip" $ do
      it "can unzip two lists of length 9" $ do
        Data.RAList.unzip ra3 `shouldBe`
          ((ra1,ra2) :: (RAList Int, RAList Char))
      it "can unzip two lists of length 3" $ do
        Data.RAList.unzip (Data.RAList.take 3 ra3) `shouldBe`
          (((Data.RAList.take 3 ra1),(Data.RAList.take 3 ra2)) ::
          (RAList Int, RAList Char))
      it "can unzip two lists of length 1" $ do
        Data.RAList.unzip (Data.RAList.take 1 ra3) `shouldBe`
          (((Data.RAList.take 1 ra1), (Data.RAList.take 1 ra2)) ::
          (RAList Int, RAList Char))
    describe "RAList.zipWith" $ do
      let ra1 = fromList [1..9]
      let ra2 = Data.RAList.replicate 9 1
      let ra3 = fromList [2,3..10]
      let f = (\x y -> x + y)
      it "can zip two lists of length 9" $ do
        Data.RAList.zipWith f ra1 ra2 `shouldBe` (ra3 :: RAList Int)
      it "cannp.zipWith two lists of length 3" $ do
        Data.RAList.zipWith f (Data.RAList.take 3 ra1) (Data.RAList.take 3 ra2)
          `shouldBe`
          ((Data.RAList.take 3 ra3) :: RAList Int)
      it "can zip two lists of length 1" $ do
        Data.RAList.zipWith f (Data.RAList.take 1 ra1) (Data.RAList.take 2 ra2)
          `shouldBe`
          (Data.RAList.take 1 ra3 :: RAList Int)
      it "can zip two lists of different lengths" $ do
        Data.RAList.zipWith f ra1 (Data.RAList.take 5 ra2) `shouldBe`
          (Data.RAList.take 5 ra3 :: RAList Int)
      it "can zip a list to an empty list" $ do
        Data.RAList.zipWith f empty ra2 `shouldBe` (empty :: RAList Int)
      it "can zip two empty lists" $ do
        Data.RAList.zipWith f empty empty `shouldBe`
          (empty :: RAList Int)

  describe "RAList.update" $ do
    let ra3 = fromList [1..3]
    let ra9 = fromList [1..9]
    it "can update the only value in a list" $ do
      Data.RAList.update 0 2 (fromList [1]) `shouldBe`
        (fromList [2] :: RAList Int)
    it "can update the first value in a list of length 3" $ do
      Data.RAList.update 0 4 ra3 `shouldBe` (fromList [4,2,3] :: RAList Int)
    it "can update the last value in a list of length 3" $ do
      Data.RAList.update 2 4 ra3 `shouldBe` (fromList [1,2,4] :: RAList Int)
    it "can update the first value in a list of length 9" $ do
      Data.RAList.update 0 10 ra9 `shouldBe`
        (fromList [10,2,3,4,5,6,7,8,9] :: RAList Int)
    it "can update the second value in a list of length 9" $ do
      Data.RAList.update 1 10 ra9 `shouldBe`
        (fromList [1,10,3,4,5,6,7,8,9] :: RAList Int)
    it "can update the third value in a list of length 9" $ do
      Data.RAList.update 2 10 ra9 `shouldBe`
        (fromList [1,2,10,4,5,6,7,8,9] :: RAList Int)
    it "can update the fourth value in a list of length 9" $ do
      Data.RAList.update 3 10 ra9 `shouldBe`
        (fromList [1,2,3,10,5,6,7,8,9])
    it "can update the last value in a list of length 9" $ do
      Data.RAList.update 8 10 ra9 `shouldBe`
        (fromList [1,2,3,4,5,6,7,8,10])
    it "throws an error when trying to update an index that is too large" $ do
      evaluate (Data.RAList.update 10 10 ra9 )`shouldThrow` anyException
    it "throws an error when trying to update an empty list" $ do
      evaluate (Data.RAList.update 0 0 empty) `shouldThrow` anyException

  describe "RAList.toList" $ do
    it "converts a list of length 1" $ do
      toList (fromList [1]) `shouldBe` ([1] :: [Int])
    it "converts a list of length 3" $ do
      toList (fromList[1..3]) `shouldBe` ([1..3] :: [Int])
    it "converts a list of length 9" $ do
      toList (fromList [1..9]) `shouldBe` ([1..9] :: [Int])
    it "converts an empty list" $ do
      toList (empty) `shouldBe` ([] :: [Int])

