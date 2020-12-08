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

-- not working yet; not sure why
main = do
    let color = "shiny gold"
    putStrLn $ "possible bags to contain a shiny gold bag in the input:\n" ++
        (show $ map bag $ getAllBagRules color $ map (fromJust . parseRule) dummyInput)
    
    contents <- readFile "7.input"
    let rules = map parseRule $ lines contents
        colors = getAllBagRules color $ map fromJust rules
    putStrLn $ "all parseable: " ++ (show $ all isJust rules)
    putStrLn $ "length: " ++ (show $ length rules)
    --putStrLn $ "possible bag colors: " ++ (show $ colors)
    putStrLn $ "possible bag colors: " ++ (show $ length colors)

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

    {-
test rules = fix (\b -> sort
                $ distinct
                $ intercalate []
                $ map (`searchForBagRules` rules) b)
                -}
    {-

searchAllBags :: Bag -> [Rule] -> [Bag]
searchAllBags bag rules = fix (\rec bs ->
                    let x = sort
                            $ distinct 
                            $ (++bs) 
                            $ intercalate []
                            $ map (\x -> searchForBagDirect x rules)
                            $ filter (/=bag) bs
                     in if bs == x then bs else rec x) (searchForBagDirect bag rules) 
                            --(searchForBagDirect bag rules)
-}


getAllBagRules :: Bag -> [Rule] -> [Rule]
getAllBagRules b r = fix 
                    (\rec n -> 
                        let comb = sort $ distinct $ n ++ res
                            res = intercalate [] 
                                $ map (\(Rule x _) -> searchForBagRules x r) n
                         in if n == comb then comb else rec comb) $ searchForBagRules b r

searchForBagRules :: Bag -> [Rule] -> [Rule]
searchForBagRules b bs = firstInstances
    where
        getRulesFor x bs = filter (\(Rule _ bs') -> elem x $ map fst bs') $ filter ((/=x) . bag)  bs
        firstInstances = getRulesFor b bs 
