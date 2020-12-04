{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns     #-}

import System.IO.Unsafe
import Control.Exception
import Data.Maybe
import Lib

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

