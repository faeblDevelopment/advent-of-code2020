{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Lib  where
import Control.DeepSeq
import Control.Exception
import System.IO.Unsafe
import Data.Maybe

splitOn :: (Eq a) => [a] -> [a] -> [[a]]
splitOn str []   = [str]
splitOn str stop = reverse $ go str stop id
    where 
        go :: Eq a => [a] -> [a] -> ([[a]] -> [[a]]) -> [[a]]
        go []  _    f = f []
        go str stop f = let (evtl, rest) = break (== (head stop)) str
                      in if same stop rest == stop 
                            then go (drop (length stop) rest) stop $ (([]:) . (addToHead evtl) . f)
                            else go (drop (length $ same stop rest) rest) stop 
                                    $ (addToHead (evtl ++ (same rest stop)) . f) 
            where addToHead :: [a] -> [[a]] -> [[a]]
                  addToHead a []     = [a]
                  addToHead a (as:r) = (as ++ a):r

                  same :: Eq a => [a] -> [a] -> [a]
                  same a b = map fst . takeWhile (uncurry (==)) $ zip a b


cReplace :: Eq a => a -> a -> [a] -> [a]
cReplace r n = map (\c -> if c == r then n else c)

deepEval :: (NFData a) => a -> Maybe a
deepEval = deepTry id

deepTry :: (NFData b) => (a -> b) -> a -> Maybe b
deepTry f a = case unsafePerformIO $ try $ evaluate $ force (f a) of
                Left (ex::SomeException) -> Nothing
                Right val -> Just val

tryParse :: forall a. (Read a,NFData a) => String -> Maybe a
tryParse s = deepTry read s




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

-- | essentially `nub`; also \(\mathcal{O}(n^2)\), 
--   but nice because implemented using set functions =P
distinct :: (Eq a) => [a] -> [a]
distinct = fromSet . toSet
