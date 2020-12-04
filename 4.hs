{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables#-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
import Lib
import Control.DeepSeq
import GHC.Generics
import Control.Exception
import System.IO.Unsafe
import Data.Maybe
import Data.List
import Data.Char
import Debug.Trace

dummyInput = unlines $  [ "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd"
                        , "byr:1937 iyr:2017 cid:147 hgt:183cm"
                        , ""
                        , "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884"
                        , "hcl:#cfa07d byr:1929"
                        , ""
                        , "hcl:#ae17e1 iyr:2013"
                        , "eyr:2024"
                        , "ecl:brn pid:760753108 byr:1931"
                        , "hgt:179cm"
                        , ""
                        , "hcl:#cfa07d eyr:2025 pid:166559648"
                        , "iyr:2011 ecl:brn hgt:59in" ]

dummyInvalid = unlines $ ["eyr:1972 cid:100"
                        , "hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926"
                        , ""
                        , "iyr:2019"
                        , "hcl:#602927 eyr:1967 hgt:170cm"
                        , "ecl:grn pid:012533040 byr:1946"
                        , ""
                        , "hcl:dab227 iyr:2012"
                        , "ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277"
                        , ""
                        , "hgt:59cm ecl:zzz"
                        , "eyr:2038 hcl:74454a iyr:2023"
                        , "pid:3556412378 byr:2007"]

dummyValid = unlines $ [ "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980"
                       , "hcl:#623a2f"
                       , ""
                       , "eyr:2029 ecl:blu cid:129 byr:1989"
                       , "iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm"
                       , ""
                       , "hcl:#888785"
                       , "hgt:164cm byr:2001 iyr:2015 cid:88"
                       , "pid:545766238 ecl:hzl"
                       , "eyr:2022"
                       , ""
                       , "iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"]


main = do
    let toStrings s = map (cReplace '\n' ' ') $ s `splitOn` "\n\n"
    putStrLn $ "correct from dummy input: " ++ 
                (show $ length $ filter isJust $ map parsePassport (toStrings dummyInput))
    contents <- readFile "4.input"
    putStrLn $ "correct from file: " ++
                (show $ length $ filter isJust $ map parsePassport (toStrings contents))

    putStrLn $ "all invalid invalid: " ++ 
                (show $ all (not . isJust) $ map parsePassport (toStrings dummyInvalid))
    putStrLn $ "all valid valid: " ++ 
                (show $ all (isJust) $ map parsePassport (toStrings dummyValid))

data Measure = CM | IN deriving (Read, Show, Generic)
data EyeColor = AMB | BLU | BRN | GRY | GRN | HZL | OTH deriving (Read, Show, Generic)

instance NFData Measure 
instance NFData EyeColor

data Passport = Passport { ecl :: !EyeColor
                         , pid :: !String
                         , eyr :: !Int
                         , hcl :: !String
                         , byr :: !Int
                         , iyr :: !Int
                         , cid :: !(Maybe Int)
                         , hgt :: !(Int, Measure)}
                         deriving Show

-- parses a space ' ' delimited list of key value pairs
parsePassport :: String -> Maybe Passport
parsePassport s = let listPairs :: [[String]]
                      listPairs = (map (`splitOn` ":")) . (`splitOn` " ") $  s
                      pairs :: [(String, String)]
                      pairs = strictMap (\(a:b:_) -> ((,) $! a) $! b) 
                                            $ filter ((==2) .length)  listPairs
                      pass :: [(String, String)] -> Maybe Passport
                      pass pairs = let 
                                        ecl' = (tryParse @EyeColor . map toUpper) 
                                                    =<< getFromMap "ecl" pairs
                                        pid' = getFromMap "pid" pairs
                                        eyr' = tryParse @Int =<< getFromMap "eyr" pairs
                                        hcl' = getFromMap "hcl" pairs
                                        byr' :: Maybe Int
                                        byr' = tryParse @Int =<< getFromMap "byr" pairs
                                        iyr' = tryParse @Int =<< getFromMap "iyr" pairs
                                        cid' = return $ tryParse @Int =<< getFromMap "cid" pairs
                                        hgt' = (\s -> let m = map toUpper 
                                                                    . reverse 
                                                                    . take 2 
                                                                    . reverse $ s
                                                          me = tryParse @Measure m
                                                          num = reverse . drop 2 . reverse $ s
                                                          nnum = tryParse @Int num
                                                     in (,) <$> nnum <*> me
                                               ) =<< getFromMap "hgt" pairs
                                        checks :: [Maybe Bool]
                                        checks = [fmap (btwIncl 1920 2002) byr'
                                                 ,fmap (btwIncl 2010 2020) iyr'
                                                 ,fmap (btwIncl 2020 2030) eyr'
                                                 ,fmap ((\h -> length h == 7
                                                          && head h == '#'
                                                          && ((==6) 
                                                                . length 
                                                                . filter (`elem` "0123456789abcdef") 
                                                                $ h))
                                                       ) hcl'
                                                 ,fmap ((\n -> length n == 9 
                                                          && all (`elem` "0123456789") n))
                                                    pid'
                                                 ,fmap (\(i,m) -> case m of
                                                                        CM -> btwIncl 150 193 i
                                                                        IN -> btwIncl 59 76 i) 
                                                    hgt']
                                        create = 
                                            if all (==Just True) checks 
                                                then Just Passport
                                                else Nothing  
                                   in create <*> ecl' <*> pid' <*> eyr' <*> hcl' 
                                                 <*> byr' <*> iyr' <*> cid' <*> hgt'
                  in pass pairs

btwIncl :: Ord a => a -> a -> a -> Bool
btwIncl l u a = a >= l && a <= u

getFromMap :: Eq a => a -> [(a, b)] -> Maybe b
getFromMap _ [] = Nothing
getFromMap a ((a',b):as) 
    | a == a' = Just $ b `seq` b
    | otherwise = getFromMap a as
                      
strictMap :: (a -> b) -> [a] -> [b]
strictMap f [] = []
strictMap f (a:as) = (f $! a) : (strictMap f as)
