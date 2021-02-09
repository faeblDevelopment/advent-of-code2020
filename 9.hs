{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Lib
import Debug.Trace
import Control.Monad (guard)
import Data.List (intercalate)

data Input = Input { numbers :: [Int]
                   , preSz   :: Int   } deriving (Show) 

dummyNums = [ 35
            , 20
            , 15
            , 25
            , 47
            , 40
            , 62
            , 55
            , 65
            , 95
            , 102
            , 117
            , 150
            , 182
            , 127
            , 219
            , 299
            , 277
            , 309
            , 576]

dummyInput = Input dummyNums 5

main = do
    let dummyResult = head $ getWrongNumbers dummyInput
    print $ dummyResult 
   
    contents <- readFile "9.input"
    let input = Input (map (read @Int) $ lines contents) 25
        result = head $ getWrongNumbers input
    print $ result
    
    let dummySum = continuousSum dummyNums $ snd dummyResult
        dummyMinMax = (snd <$> dummySum) >>= minMax
    print dummySum
    print dummyMinMax
    print $ (uncurry (+)) <$> dummyMinMax

    let sum' = continuousSum (numbers input) $ snd result
        minMax' = (snd <$> sum') >>= minMax
    print sum'
    print minMax'
    print $ (uncurry (+)) <$> minMax'


newtype Index = Idx Int deriving Show 
newtype Window = Win Int deriving Show


minMax :: (Ord a) => [a] -> Maybe (a,a)
minMax [] = Nothing
minMax (a:[]) = Just (a,a)
minMax l = Just (minimum l, maximum l)

continuousSum :: (Eq a, Num a) => [a] -> a -> Maybe (a, [a])
continuousSum l i = safeHead 
                  $ filter (\(s,l') -> s == i) 
                  $ map (\l' -> (sum l', l')) 
                  $ substrings l

substrings :: [a] -> [[a]]
substrings [] = []
substrings l = filter ((>0) . length) 
             $ intercalate []
             $ map (\i -> 
                    let sublist = drop i l
                     in map (\j -> take j sublist) [0..length sublist]) [0..length l]

getWrongNumbers :: Input -> [(Index, Int)]
getWrongNumbers (Input [] _)          = []
getWrongNumbers (Input _  a) | a <= 0 = []
getWrongNumbers (Input nums d) = map (\(i,n,b) -> (i,n)) 
                               $ filter (\(i,n,b) -> not b) 
                               $ map (\x -> (Idx x, nums!!x, validate nums (Win d) (Idx x))) 
                                        [d..(length nums)-1]

        
validate :: [Int] -> Window -> Index -> Bool
validate nums win@(Win w) idx@(Idx i) = 
    or $ map (\(a,b) -> a+b == nums!!i) $ getPairs $ window nums win idx

window :: [Int] -> Window -> Index -> [Int]
window _ _ (Idx 0) = []
window nums (Win w) (Idx i) = let start = drop (i-w) nums
                               in if w >= i 
                                     then init $ take i start
                                     else take w start 

getPairs :: (Eq a) => [a] -> [(a,a)]
getPairs l = do
                xs <- l
                ys <- l
                guard (xs /= ys) 
                return (xs,ys)


