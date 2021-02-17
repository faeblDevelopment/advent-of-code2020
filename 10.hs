{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Lib
import Debug.Trace
import Data.List (sort)

dummyAdapters = [ 16
                , 10
                , 15
                , 5
                , 1
                , 11
                , 7
                , 19
                , 6
                , 12
                , 4 ]

dummyAdapters2 = [ 28
                 , 33
                 , 18
                 , 42
                 , 31
                 , 14
                 , 46
                 , 20
                 , 48
                 , 47
                 , 24
                 , 23
                 , 49
                 , 45
                 , 19
                 , 38
                 , 39
                 , 11
                 , 1
                 , 32
                 , 25
                 , 35
                 , 8
                 , 17
                 , 7
                 , 9
                 , 4
                 , 2
                 , 34
                 , 10
                 , 3 ]

main = do
   
    let counts l = map (\x -> (x, length $ filter (==x) l)) [0..3]
        calcResult l = snd . head $ scanr (\a (b,l) -> (a ,(b-a):l)) (maximum dummyAdapters + 3,[]) $ sort $ 0:l
        dummyResult = calcResult dummyAdapters
    print $ counts dummyResult
    print $ counts $ calcResult dummyAdapters2

    contents <- readFile "10.input"
    let adapters = map (read @Int) $ lines contents
    print $ counts $ calcResult adapters -- have to add +1 to the count for three, not yet sure why

