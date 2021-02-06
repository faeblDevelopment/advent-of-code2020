{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Data.List
import Control.DeepSeq
import GHC.Generics
import Data.Maybe
import Debug.Trace
import Data.Function
import Lib

dummyInput = ["light red bags contain 1 bright white bag, 2 muted yellow bags."
             ,"dark orange bags contain 3 bright white bags, 4 muted yellow bags."
             ,"bright white bags contain 1 shiny gold bag."
             ,"muted yellow bags contain 2 shiny gold bags, 9 faded blue bags."
             ,"shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."
             ,"dark olive bags contain 3 faded blue bags, 4 dotted black bags."
             ,"vibrant plum bags contain 5 faded blue bags, 6 dotted black bags."
             ,"faded blue bags contain no other bags."
             ,"dotted black bags contain no other bags."]

dummy2 = [ "shiny gold bags contain 2 dark red bags."
         , "dark red bags contain 2 dark orange bags."
         , "dark orange bags contain 2 dark yellow bags."
         , "dark yellow bags contain 2 dark green bags."
         , "dark green bags contain 2 dark blue bags."
         , "dark blue bags contain 2 dark violet bags."
         , "dark violet bags contain no other bags." ]


main = do
    let color = "shiny gold"
        rule = getFirst (\(Rule x _) -> x == color)
    putStrLn $ "possible bags to contain a shiny gold bag in the input:\n" ++
        (show $ map bag $ getAllBagRules color $ map (fromJust . parseRule) dummyInput)
    
    contents <- readFile "7.input"
    let rules = map parseRule $ lines contents
        colors = getAllBagRules color $ map fromJust rules
    putStrLn $ "all parseable: " ++ (show $ all isJust rules)
    putStrLn $ "length: " ++ (show $ length rules)
    --putStrLn $ "possible bag colors: " ++ (show $ colors)
    putStrLn $ "possible bag colors: " ++ (show $ length colors)

    ---
    let toRules = map (fromJust . parseRule)
        dummyRules = toRules dummyInput 
        dummy2Rules = toRules dummy2
        inputRules = map fromJust rules
    putStrLn $ "numberOfBags in dummy inpuy: " ++
        (show $ getContainedBags (rule $ dummyRules) dummyRules)
    putStrLn $ "numberOfBags in dummy 2: " ++
        (show $ getContainedBags (rule $ dummy2Rules) dummy2Rules)
    putStrLn $ "numberOfBags in input: " ++
        (show $ getContainedBags (rule $ inputRules) inputRules) 
    
type Bag = String

data Rule = Rule { bag :: !Bag
                 , spec :: ![(Bag, Int)] }
                 deriving (Show, Eq, Generic)
instance NFData Rule
instance Ord Rule where
    (Rule a _) <= (Rule b _) = a <= b

without :: Eq a => [a] -> [a] -> [a]
without s w = intercalate [] $ s `splitOn` w

takeInts :: String -> Maybe Int
takeInts = deepTry read . (takeWhile (`elem` ['0'..'9']))

contains :: Eq a => [a] -> [a] -> Bool
contains a b = length a /= length (a `without` b)

cleanWhitespace :: String -> String
cleanWhitespace = reverse . dropWhile (`elem` " \n\r") . reverse . dropWhile (`elem` " \n\r")

dropInts :: String -> String
dropInts = (dropWhile (`elem` ['0'..'9']))

parseRule :: String -> Maybe Rule
parseRule s = let 
                cleaned = (`without` ".") . (`without` " bag") . (`without` " bags") $ s
                (b:specs:_) = cleaned `splitOn` " contain "
                specList :: [(Bag, Int)]
                specList = map (\s -> (cleanWhitespace $ dropInts s, fromJust $ takeInts s)) $ specs `splitOn` ", "
              in deepEval (Rule (cleanWhitespace b) $ if s `contains` " contain no other bags" then [] else specList)
       
trace' :: Show a => a -> a
trace' !a = trace (show a) a


getAllBagRules :: Bag -> [Rule] -> [Rule]
getAllBagRules b r = fix 
                    (\rec n -> 
                        let comb = sort $ distinct $ n ++ res
                            res = intercalate [] 
                                $ map (\(Rule x _) -> searchForBagRules x r) n
                         in if n == comb then comb else rec comb) $ searchForBagRules b r

getFirst :: (a -> Bool) -> [a] -> a
getFirst f = head . filter f

getContainedBags :: Rule -> [Rule] -> Int
getContainedBags b r = fix
                    (\rec (Rule n bs) ->
                        sum $ map (\(b', n) -> 
                                    n * (1+ rec (getFirst (\(Rule x _) -> x == b') r))) bs) 
                         $ b

searchForBags :: Bag -> [Rule] -> [(Bag, Int)]
searchForBags b bs = firstInstances
    where   
        getBagsFor x bs = filter (\(Rule b' _) -> x == b') bs
        firstInstances = intercalate [] $ map spec $ getBagsFor b bs

searchForBagRules :: Bag -> [Rule] -> [Rule]
searchForBagRules b bs = firstInstances
    where
        getRulesFor x bs = filter (\(Rule _ bs') -> elem x $ map fst bs') $ filter ((/=x) . bag)  bs
        firstInstances = getRulesFor b bs 


