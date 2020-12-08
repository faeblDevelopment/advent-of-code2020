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
