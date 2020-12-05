{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}

import Data.List 
import Control.DeepSeq
import GHC.Generics
import Debug.Trace
import Data.Maybe
import Lib

dummyInput = [("BFFFBBFRRR", 567)
             ,("FFFBBBFRRR", 119)
             ,("BBFFBBFRLL", 820)
             ,("FBFBBFFRLR", 357)]


main = do
    putStrLn $ "all dummy IDs correct? " ++
        (show $ and
              $ zipWith (==) ((map (return . toInteger . snd) dummyInput) :: [Maybe Integer])
                             (map (fmap seatID . parseSeat (128,8) . fst) dummyInput))
    contents <- readFile "5.input"
    let ids = (map (fmap seatID . parseSeat (128,8)) $ lines contents)
        allOk = all isJust ids
    putStrLn $ "all valid: " ++
        (show $ allOk) 

    putStrLn $ "maximum id: " ++ 
        (show $ maximum $ map fromJust ids)

    let allSeats = [ (r*8+c) | r <- [1..127], c <- [0..7]] 
    putStrLn $ "seat: " ++
        (show $ reverse $ dropWhileNext (\i -> i-1) $ reverse
              $ dropWhileNext (+1) 
              $ filter (not . (`elem` (map fromJust $ ids))) $ allSeats) 

dropWhileNext :: (Num a, Eq a) => (a -> a) -> [a] -> [a]
dropWhileNext _ []  = []
dropWhileNext f (h:t) = go f h t
    where
        go :: (Num a, Eq a) => (a->a) -> a -> [a] -> [a]
        go f l [] = []
        go f l li@(h:t)
          | f l == h = go f h t
          | otherwise = li


data Row = F | B deriving (Show, Read, Eq, Generic)
data Col = L | R deriving (Show, Read, Eq, Generic)

instance NFData Row
instance NFData Col

data Seat = Seat { row :: ![Row]
                 , col :: ![Col]
                 , numRows :: !Int
                 , numCols :: !Int }
    deriving Show

type Plane = (Int, Int)
both :: (a -> Bool) -> (a,a) -> Bool
both f (a,b) = f a && f b

seatID :: Seat -> Integer
seatID (Seat r c nr nc) = rowNum * 8 + colNum
    where
        calcFor f l = ((if last l == f then fst else snd) . (foldr (flip (.)) id 
                        $ map (halfArea f) $ init l)) 

        halfArea f = \r (l,u) ->
                    let delta :: Double 
                        delta = (fromIntegral $ u - l)/2 in
                    if r == f 
                        then (l, toInteger $ (l+floor delta))
                        else (u-(floor delta), toInteger u)
                 
        rowNum = calcFor F r $ (fromIntegral 0, fromIntegral $ nr-1)
        colNum = calcFor L c $ (fromIntegral 0, fromIntegral $ nc-1)


bimapM :: forall b d e f a c. (a -> e b) -> (c -> f d) -> (a,c) -> (e b,f d)
bimapM fa fc (a,c) = (fa a, fc c)

bimap :: forall b d a c. (a -> b) -> (c -> d) -> (a,c) -> (b,d)
bimap fa fc (a,c) = (fa a, fc c)

parseSeat :: Plane -> String -> Maybe Seat
parseSeat (rows,cols) s
  | length s /= lenRows + lenCols = Nothing
  | otherwise = let
                    parseRow = map (tryParse @Row)
                    parseCol = map (tryParse @Col)
                    (rs',cs') = bimapM parseRow parseCol parts 
                in (\r s -> Seat r s rows cols) <$> (sequence rs') <*> (sequence cs')
      where 
          sParts = splitAt lenRows s
          parts :: ([String], [String])
          parts@(rowPart,colPart) = bimapM (map return) (map return) sParts
          lenRows :: Int
          lenRows = fromIntegral $ floor $ logBase (fromIntegral 2) (fromIntegral rows)
          lenCols :: Int
          lenCols = fromIntegral $ floor $ logBase (fromIntegral 2) (fromIntegral cols)
