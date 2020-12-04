{-# LANGUAGE ScopedTypeVariables #-}
module Lib  where
import Control.DeepSeq
import Control.Exception
import System.IO.Unsafe

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


tryParse :: forall a. (Read a,NFData a) => String -> Maybe a
tryParse s = case (unsafePerformIO $ try $ evaluate $ force $ ((read s)::a)) of
                  Left (ex::SomeException) -> Nothing
                  Right int -> Just int
