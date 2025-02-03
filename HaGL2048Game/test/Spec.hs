import Lib
import Data.List (sortBy, unfoldr)
import System.Random
import System.CPUTime

expectEqual :: (Eq a, Show a) => a -> a -> String
expectEqual a b = if (a == b) then "Pass" else ("Fail \n\t value:" ++ (show a) ++ " not equal to expect:" ++ (show b))

tests = [
    ("testStillMotion",       expectEqual (stillMotion (TL (0, 0) 1))          (LM (-0.75, -0.75) (-0.75, -0.75) 1 1)),

    ("testEnMotion_NoMotion", expectEqual (enMotion (TL (0, 0) 1) (TL (0, 0) 1)) (LM (-0.75, -0.75) (-0.75, -0.75) 1 1)),
    ("testEnMotion_XMotion",  expectEqual (enMotion (TL (0, 0) 1) (TL (1, 0) 1)) (LM (-0.75, -0.75) (-0.25, -0.75) 1 1)),
    ("testEnMotion_YMotion",  expectEqual (enMotion (TL (0, 0) 1) (TL (0, 1) 1)) (LM (-0.75, -0.75) (-0.75, -0.25) 1 1)),
    
    ("testPredDir_XDirTrue",  expectEqual (predDir (-1, 0) (TL (0, 2) 1) (TL (1, 2) 2)) True),
    ("testPredDir_XDirFalse", expectEqual (predDir (-1, 0) (TL (0, 0) 1) (TL (0, 2) 2)) False),
    ("testPredDir_YDirTrue",  expectEqual (predDir ( 0, 1) (TL (1, 0) 1) (TL (1, 2) 2)) True),
    ("testPredDir_YDirFalse", expectEqual (predDir ( 0, 1) (TL (2, 2) 1) (TL (1, 2) 2)) False),

    -- splitDir :: (Int, Int) -> [Tile] -> [[Tile]]
    ("testSplitDir_OneChunk", expectEqual (splitDir (-1, 0) [TL (1, 3) 1, TL (2, 3) 1]) [[TL (1, 3) 1, TL (2, 3) 1]]),
    ("testSplitDir_TwoChunk", expectEqual (splitDir ( 0, 1) [TL (2, 3) 1, TL (1, 3) 1]) [[TL (2, 3) 1], [TL (1, 3) 1]]),
    ("testSplitDir_TwoChunk", expectEqual (splitDir ( 1, 0) [TL (0, 3) 1, TL (0, 2) 1]) [[TL (0, 3) 1], [TL (0, 2) 1]]),

    -- dirNum :: (Int, Int) -> [(Int, Int)]
    ("testDirNum_+X",          expectEqual (dirNum ( 1, 0)) [(3, 0), (2, 0), (1, 0), (0, 0)]),
    ("testDirNum_-X",          expectEqual (dirNum (-1, 0)) [(0, 0), (1, 0), (2, 0), (3, 0)]),

    -- moveTiles :: (Int, Int) -> [Tile] -> ([Tile], [LinearMotion], Bool)
    ("testMoveTiles_+x",       expectEqual (moveTiles False ( 1,  0) [TL (0, 0) 1]) ([TL (3,0) 1], [LM (-0.75,-0.75) (0.75,-0.75) 1 1], True)),
    ("testMoveTiles_-x",       expectEqual (moveTiles False (-1,  0) [TL (3, 0) 1]) ([TL (0,0) 1], [LM (0.75,-0.75) (-0.75,-0.75) 1 1], True)),
    ("testMoveTiles_2+x",      expectEqual (moveTiles False ( 1,  0) [TL (0, 3) 1, TL (0, 2) 1]) ([TL (3, 2) 1, TL (3,3) 1], [enMotion (TL (0, 2) 1) (TL (3, 2) 1), enMotion (TL (0, 3) 1) (TL (3, 3) 1)], True)),
    ("testMoveTiles_2-y",      expectEqual (moveTiles False ( 0, -1) [TL (0, 3) 1, TL (0, 2) 1]) ([TL {pos = (0,0), val = 2}],[LM (-0.75,0.25) (-0.75,-0.75) 1 2, LM (-0.75,0.75) (-0.75,-0.75) 1 2],True)),

    -- sortBy (sortDir dir) tiles
    ("sortTiles-y",            expectEqual (sortBy (sortDir (0, -1))  [TL (0, 3) 1, TL (0, 2) 1, TL (1, 2) 1]) ([TL (0, 2) 1, TL (0, 3) 1, TL (1, 2) 1])),
    ("sortTiles+y",            expectEqual (sortBy (sortDir (0,  1))  [TL (0, 3) 1, TL (0, 2) 1, TL (1, 2) 1]) ([TL (0, 3) 1, TL (0, 2) 1, TL (1, 2) 1])),

    ("testMoveTiles_3-y_consolidate",        expectEqual ((\(a, b, c) -> a)$ moveTiles False ( 0, -1) [TL (0, 3) 1, TL (0, 2) 1, TL (1, 2) 1]) ([TL (0, 0) 2, TL (1,0) 1]))
    ]

runTest (nm, m) = putStrLn (nm ++ " -> " ++ m)

-- unfoldr :: (b -> Maybe (a, b)) -> b -> [a]

testRandom :: IO()
testRandom = do
    tim <- getCPUTime
    print $ take 5 $ unfoldr (randStep (1, 10)) (mkStdGen (fromIntegral $ tim `mod` 137))
    where
        randStep :: (Word, Word) -> StdGen -> Maybe (Word, StdGen)
        randStep range gen = Just $ randomR range gen

main :: IO ()
main = do
    mapM_ runTest tests
    testRandom
