import Lib

expectEqual :: (Eq a, Show a) => a -> a -> String
expectEqual a b = if (a == b) then "Pass" else ("Fail \n\t value:" ++ (show a) ++ " not equal to expect:" ++ (show b))

tests = [
    ("testStillMotion",       expectEqual (stillMotion (TL (0, 0)))          (LM (-0.75, -0.75) (-0.75, -0.75))),

    ("testEnMotion_NoMotion", expectEqual (enMotion (TL (0, 0)) (TL (0, 0))) (LM (-0.75, -0.75) (-0.75, -0.75))),
    ("testEnMotion_XMotion",  expectEqual (enMotion (TL (0, 0)) (TL (1, 0))) (LM (-0.75, -0.75) (-0.25, -0.75))),
    ("testEnMotion_YMotion",  expectEqual (enMotion (TL (0, 0)) (TL (0, 1))) (LM (-0.75, -0.75) (-0.75, -0.25))),
    
    ("testPredDir_XDirTrue",  expectEqual (predDir (-1, 0) (TL (0, 2)) (TL (1, 2))) True),
    ("testPredDir_XDirFalse", expectEqual (predDir (-1, 0) (TL (0, 0)) (TL (0, 2))) False),
    ("testPredDir_YDirTrue",  expectEqual (predDir ( 0, 1) (TL (1, 0)) (TL (1, 2))) True),
    ("testPredDir_YDirFalse", expectEqual (predDir ( 0, 1) (TL (2, 2)) (TL (1, 2))) False),

    -- splitDir :: (Int, Int) -> [Tile] -> [[Tile]]
    ("testSplitDir_OneChunk", expectEqual (splitDir (-1, 0) [TL (1, 3), TL (2, 3)]) [[TL (1, 3), TL (2, 3)]]),
    ("testSplitDir_TwoChunk", expectEqual (splitDir ( 0, 1) [TL (2, 3), TL (1, 3)]) [[TL (2, 3)], [TL (1, 3)]]),
    ("testSplitDir_TwoChunk", expectEqual (splitDir ( 1, 0) [TL (0, 3), TL (0, 2)]) [[TL (0, 3)], [TL (0, 2)]]),

    -- dirNum :: (Int, Int) -> [(Int, Int)]
    ("testDirNum_+X",          expectEqual (dirNum ( 1, 0)) [(3, 0), (2, 0), (1, 0), (0, 0)]),
    ("testDirNum_-X",          expectEqual (dirNum (-1, 0)) [(0, 0), (1, 0), (2, 0), (3, 0)]),

    -- moveTiles :: (Int, Int) -> [Tile] -> ([Tile], [LinearMotion], Bool)
    ("testMoveTiles_+x",         expectEqual (moveTiles ( 1,  0) [TL (0, 0)]) ([TL (3,0)], [LM (-0.75,-0.75) (0.75,-0.75)], True)),
    ("testMoveTiles_-x",         expectEqual (moveTiles (-1,  0) [TL (3, 0)]) ([TL (0,0)], [LM (0.75,-0.75) (-0.75,-0.75)], True)),
    ("testMoveTiles_2+x",        expectEqual (moveTiles ( 1,  0) [TL (0, 3), TL (0, 2)]) ([TL (3, 2), TL (3,3)], [enMotion (TL (0, 2)) (TL (3, 2)), enMotion (TL (0, 3)) (TL (3, 3))], True)),
    ("testMoveTiles_2-y",        expectEqual (moveTiles ( 0, -1) [TL (0, 3), TL (0, 2)]) ([TL (0, 0), TL (0,1)], [enMotion (TL (0, 2)) (TL (0, 0)), enMotion (TL (0, 3)) (TL (0, 1))], True))
    ]

runTest (nm, m) = putStrLn (nm ++ " -> " ++ m)

main :: IO ()
main = mapM_ runTest tests
