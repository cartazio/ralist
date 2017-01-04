{-#LANGUAGE Safe #-}
import Criterion.Main
import Data.RAList

hundred :: RAList Int
hundred = fromList [0..100]

thousand :: RAList Int
thousand = fromList [0..1000]

tenThousand :: RAList Int
tenThousand = fromList [0..10000]

hundredThousand :: RAList Int
hundredThousand = fromList [0..100000]

million :: RAList Int
million = fromList [0..1000000]

tenMillion :: RAList Int
tenMillion = fromList [0..10000000]

main = defaultMain [
    bgroup "drop"
        [ bench "TenThousand" $ whnf (Data.RAList.drop 100) tenThousand,
          bench "HundredThousand" $ whnf (Data.RAList.drop 100) hundredThousand,
          bench "Million" $ whnf (Data.RAList.drop 100) million,
          bench "TenMillion" $ whnf (Data.RAList.drop 100) tenMillion,

          bench "TenThousand-Drop1" $ whnf (Data.RAList.drop 1) tenThousand,
          bench "HundredThousand-Drop1" $ whnf (Data.RAList.drop 1) hundredThousand,
          bench "Million-Drop1" $ whnf (Data.RAList.drop 1) million,
          bench "TenMillion-Drop1" $ whnf (Data.RAList.drop 1) tenMillion
        ],

    bgroup "simpleDrop"
        [ bench "TenThousand" $ whnf (Data.RAList.simpleDrop 100) tenThousand,
          bench "HundredThousand" $ whnf (Data.RAList.simpleDrop 100) hundredThousand,
          bench "Million" $ whnf (Data.RAList.simpleDrop 100) million,
          bench "TenMillion" $ whnf (Data.RAList.simpleDrop 100) tenMillion,

          bench "TenThousand-Drop1" $ whnf (Data.RAList.simpleDrop 1) tenThousand,
          bench "HundredThousand-Drop1" $ whnf (Data.RAList.simpleDrop 1) hundredThousand,
          bench "Million-Drop1" $ whnf (Data.RAList.simpleDrop 1) million,
          bench "TenMillion-Drop1" $ whnf (Data.RAList.simpleDrop 1) tenMillion
        ],

    bgroup "cons"
        [ bench "hundred" $ whnf (Data.RAList.cons 0) hundred,
          bench "thousand" $ whnf (Data.RAList.cons 0) thousand
        ],

    bgroup "uncons"
        [ bench "hundred" $ whnf Data.RAList.uncons hundred,
          bench "thousand" $ whnf Data.RAList.uncons thousand
        ],
    bgroup "lookup last element"
        [ bench "TenThousand" $ whnf  (tenThousand Data.RAList.!!) 10000,
          bench "HundredThousand" $ whnf (hundredThousand Data.RAList.!!) 100000,
          bench "Million" $ whnf (million Data.RAList.!!) 1000000,
          bench "TenMillion" $ whnf (tenMillion Data.RAList.!!) 10000000
        ] ]
