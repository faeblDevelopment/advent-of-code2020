{-# LANGUAGE TypeApplications #-}
import Data.List

dummyData :: [Int]
dummyData = [ 1721
            , 979
            , 366
            , 299
            , 675
            , 1456 ]


main = do 
        let result = dummyData `addUpTo` 2020 
        print result 
        print $ fmap (uncurry (*)) result
        print "----------------"
        file <- readFile "1.input"
        let contents = (fmap (read @Int) $ lines file)
        let res2 = contents `addUpTo` 2020
        print res2
        print $ fmap (uncurry (*)) res2
        print "----------------"
        let res3 = repAddUpTo 2020 3 contents
        print res3
        print $ fmap product res3
            {-
        print "----------------"
        let res4 = addUpTo' contents 2020 2
        print res4
        print $ fmap product res4
            -}

-- very inefficient due to the permutations call;
-- too many idential permutations of length c generated
addUpTo' :: [Int] -> Int -> Int -> Maybe [Int]
addUpTo' l i c = headMaybe 
               $ filter ((==i) . sum) 
               $ map (take c)
               $ permutations l 

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (a:_) = Just a

addUpTo :: [Int] -> Int -> Maybe (Int, Int)
addUpTo []     _ = Nothing
addUpTo (a:[]) _ = Nothing
addUpTo (h:t) i
  | length result > 0 = Just $ (h, head result)
  | otherwise         = t `addUpTo` i
    where 
        result = filter ((==i) . (+h)) t

repAddUpTo :: Int -> Int -> [Int] -> Maybe [Int]
repAddUpTo num count list = go (count-1) $ map return list
    where
        go :: Int -> [[Int]] -> Maybe [Int]
        go 0 prev = headMaybe $ 
                    filter (\y -> length y == count && sum y == num) prev
        go n' prev = go (n'-1) newPrev
            where
                prevWithCandidates :: [([Int], [Int])]
                prevWithCandidates = map (\p -> 
                    (p, filter (\l -> not (elem l p) && (sum p)+l <= num) list)
                               ) prev 
                newPrev :: [[Int]]
                newPrev = intercalate [] 
                        $ map (\(p, is) -> map (:p) is) 
                              prevWithCandidates
 
