
module Main where
import Criterion.Main
import Data.RAList

{-# NOINLINE hundred #-}
hundred :: RAList Int
hundred = fromList [0..100]

{-# NOINLINE thousand#-}
thousand :: RAList Int
thousand = fromList [0..1000]

{-# NOINLINE tenThousand#-}
tenThousand :: RAList Int
tenThousand = fromList [0..10000]

hundredThousand :: RAList Int
hundredThousand = fromList [0..100000]

--million :: RAList Int
--million = fromList [0..1000000]

--tenMillion :: RAList Int
--tenMillion = fromList [0..10000000]

main = defaultMain [
    bgroup "drop"
        [ bench "Thousand" $ whnf (Data.RAList.drop 100) thousand

          ,bench "Thousand-Drop1" $ whnf (Data.RAList.drop 1) thousand
          --bench "HundredThousand-Drop1" $ whnf (Data.RAList.drop 1) hundredThousand
          --bench "Million-Drop1" $ whnf (Data.RAList.drop 1) million,
          --bench "TenMillion-Drop1" $ whnf (Data.RAList.drop 1) tenMillion
        ],

    bgroup "simpleDrop"
        [ bench "Thousand" $ whnf (Data.RAList.simpleDrop 100) thousand

          ,bench "Thousand-Drop1" $ whnf (Data.RAList.simpleDrop 1) thousand

        ],

    bgroup "cons"
        [ bench "hundred" $ whnf (Data.RAList.cons 0) hundred
          ,bench "thousand" $ whnf (Data.RAList.cons 0) thousand
        ],

    bgroup "uncons"
        [ bench "hundred" $ whnf Data.RAList.uncons hundred
          ,bench "thousand" $ whnf Data.RAList.uncons thousand
        ],
    bgroup "lookup last element"
        [ bench "TenThousand" $ whnf  (tenThousand Data.RAList.!!) 10000
          --,bench "HundredThousand" $ whnf (hundredThousand Data.RAList.!!) 100000

        ] ]
