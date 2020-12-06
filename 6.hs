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

distinct :: (Eq a) => [a] -> [a]
distinct = fromSet . toSet

getAnswerCount :: [String] -> Int
getAnswerCount answers = length $ fromSet $ toSet $ foldr (++) "" answers  

allContain :: Eq a => [[a]] -> a -> Bool
allContain lists a = and $ map (a `elem`) lists

main = do
        let parseInputs = filter (/= "") . map (filter (/= '\n')) . (`splitOn` "\n\n") 
            dummyDistinct = map distinct $ parseInputs dummyInput
            dummyCounts = map length dummyDistinct
        
        -- dummy --
        putStrLn $ "ansers counts in dummyInput: " ++
            (show $ dummyCounts)
        putStrLn $ "ansers sum in dummyInput: " ++
            (show $ sum dummyCounts)
        
        -- input -- 
        contents <- readFile "6.input"
        let distincts = map distinct $ parseInputs contents
            counts = map length distincts

        putStrLn $ "ansers sum in Input: " ++
            (show $ sum counts)
        
        ------------------ 2 ----------------------------------------

        let parseInputs' = map (filter (/= "")) . map (`splitOn` "\n") . (`splitOn` "\n\n")
            getAnswers dist inp = map (length . filter (==True)) 
                            $ map (\(d,i) -> map (i `allContain`) d) 
                            $ zip dist $ parseInputs' inp
            dummyAnswers = getAnswers dummyDistinct dummyInput
            answers = getAnswers distincts contents

        -- dummy --
        putStrLn $ "allContains in dummyInput: " ++
            (show $ dummyAnswers)
        putStrLn $ "sum allContains in dummyInput: " ++
            (show $ sum dummyAnswers)

        -- input --
        putStrLn $ "allContains in input: " ++
            (show $ answers)
        putStrLn $ "sum allContains in input: " ++
            (show $ sum answers)
