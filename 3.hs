import Data.List (intercalate)
import Debug.Trace

dummyPattern =  [ "..##......."
                , "#...#...#.."
                , ".#....#..#."
                , "..#.#...#.#"
                , ".#...##..#."
                , "..#.##....."
                , ".#.#.#....#"
                , ".#........#"
                , "#.##...#..."
                , "#...##....#"
                , ".#..#...#.#" ]

hill :: [String] -> [String]
hill = map (intercalate [] . repeat)

main = do
    contents <- readFile "3.input"
    let h = hill $ lines $ contents
    print $ countTrees h (1,3)
    print $ countTrees (hill $ lines $ unlines $ dummyPattern) (1,3)

    print "-- second --"
    let slopes = [(1,1), (1,3), (1,5), (1,7), (2,1)]
    print $ "cheking with dummy: " ++ show (product $ map (countTrees (hill dummyPattern)) slopes)
    
    print $ product $ map (countTrees h) slopes


type Direction = (Int, Int) -- +x,+y
type Position = (Int, Int)  -- x,y

countTrees :: [String] -> Direction -> Int
countTrees s d = go s d (0,0) id
    where
        go :: [String] -> Direction -> Position -> (Int -> Int) -> Int
        go map d@(dx, dy) (x,y) f
          | x >= length map = f 0
          | otherwise = go map d (x+dx, y+dy) ((if map!!x!!y == '#' then (+1) else id) . f)
