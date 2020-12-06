{-# LANGUAGE GADTs #-}

import Data.Maybe
import Data.Kind
import Lib

dummyInput = unlines $ ["abc"
                       ,""
                       ,"a"
                       ,"b"
                       ,"c"
                       ,""
                       ,"ab"
                       ,"ac"
                       ,""
                       ,"a"
                       ,"a"
                       ,"a"
                       ,"a"
                       ,""
                       ,"b"]

data Set a where
    S :: (Eq a) => [a] -> Set a

instance Show a => Show (Set a) where
    show (S a) = "Set " ++ show a

toSet :: (Eq a) => [a] -> Set a
toSet = addToSet (S [])

addToSet :: Eq a => Set a -> [a] -> Set a
addToSet s [] = s
addToSet (S l) as = S $ l ++ (catMaybes $ foldr (\x acc -> if (x `elem` l) 
                                                                 || ((Just x) `elem` acc)
                                                      then Nothing:acc
                                                      else (Just x):acc)
                                                    []
                                                    as)
fromSet :: Set a -> [a]
fromSet (S a) = a

--| essentially `nub`; also \(\mathcal{O}(n^2)\), 
-- but nice because implemented using set functions =P
distinct :: (Eq a) => [a] -> [a]
distinct = fromSet . toSet

{-
getAnswerCount :: [String] -> Int
getAnswerCount = length . distinct . foldr1 (++)  
-}

allContain :: Eq a => [[a]] -> a -> Bool
allContain lists a = and $ map (a `elem`) lists

main = do
        let allOfGroups = filter (/= "") . map (filter (/= '\n')) . (`splitOn` "\n\n") 
            distinctForGroups = map distinct . allOfGroups
            distinctLengths = map length
            dummyDistinct = distinctForGroups dummyInput
        
        -- dummy --
        putStrLn $ "ansers counts in dummyInput: " ++
            (show $ distinctLengths $ dummyDistinct)
        putStrLn $ "ansers sum in dummyInput: " ++
            (show $ sum $ distinctLengths $ dummyDistinct)
        
        -- input -- 
        contents <- readFile "6.input"
        let distincts = distinctForGroups contents

        putStrLn $ "ansers sum in Input: " ++
            (show $ sum $ distinctLengths $ distincts)
        
        ------------------ 2 ----------------------------------------

        let parseInputs' = map (filter (/= "")) . map (`splitOn` "\n") . (`splitOn` "\n\n")
            getAnswers inp = map (length . filter (==True)) 
                                $ map (\(d,i) -> map (i `allContain`) d) 
                                $ zip (distinctForGroups inp) $ parseInputs' inp

            dummyAnswers        = getAnswers dummyInput
            answers             = getAnswers contents

        -- dummy --
        putStrLn $ "allContains in dummyInput: " ++
            (show $ dummyAnswers)
        putStrLn $ "sum allContains in dummyInput: " ++
            (show $ sum dummyAnswers)

        -- input --
        --putStrLn $ "allContains in input: " ++
        --    (show $ answers)
        putStrLn $ "sum allContains in input: " ++
            (show $ sum answers)
