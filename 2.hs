{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns     #-}
import System.IO.Unsafe
import Control.Exception
import Data.Maybe

dummyData = [ "1-3 a: abcde"
            , "1-3 b: cdefg"
            , "2-9 c: ccccccccc" ]

main = do
        contents <- readFile "2.input"
        print $ length $ filter isJust $ map (parsePassword toPassword') $ dummyData
        print $ length $ filter isJust $ map (parsePassword toPassword') $ lines contents

newtype Password = Password String deriving Show

data PasswordSpec = PasswordSpec
                    { from :: !Int
                    , to   :: !Int
                    , chr  :: !Char }
                deriving Show

toSpec :: String -> Maybe PasswordSpec
toSpec str = let parts = str `splitOn` " "
                 (from:to:_) = (parts!!0) `splitOn` "-" 
                 parsed = unsafePerformIO $ try @SomeException $ return 
                                (PasswordSpec 
                                            (read @Int from)
                                            (read @Int to)
                                            (head (parts!!1))
                                ) 
              in case parsed of
                   Left (ex) -> Nothing
                   Right spec -> Just spec

type PassFunc = String -> PasswordSpec -> Maybe Password

toPassword :: PassFunc
toPassword str spec = let chars = filter (==(chr spec)) str
                       in if length chars >= (from spec)
                            && length chars <= (to spec)
                        then Just $ Password str
                        else Nothing

toPassword' :: PassFunc
toPassword' str spec = let chars = [str!!(from spec -1), str!!(to spec -1)]
                        in if (length $ filter (==chr spec) chars ) == 1
                        then Just $ Password str
                        else Nothing


parsePassword :: PassFunc -> String -> Maybe Password
parsePassword passFunc str = let (strSpec:strPass:_) = str `splitOn` ": "
                                 spec = toSpec strSpec
                             in (passFunc strPass) =<< spec 

splitOn :: (Eq a) => [a] -> [a] -> [[a]]
splitOn str []   = [str]
splitOn str stop = reverse $ go str stop id
    where 
        go :: Eq a => [a] -> [a] -> ([[a]] -> [[a]]) -> [[a]]
        go []  _    f = f []
        go str stop f = let (evtl, rest) = break (== (head stop)) str
                      in if take (length stop) rest == stop 
                            then go (drop (length stop) rest) stop $ (([]:) . (addToHead evtl) . f)
                            else go (drop 1 rest) stop $ (addToHead (evtl) . f) 
            where addToHead :: [a] -> [[a]] -> [[a]]
                  addToHead a []     = [a]
                  addToHead a (as:r) = (a ++ as):r
