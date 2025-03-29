
expectEqual :: (Eq a, Show a) => a -> a -> String
expectEqual a b = if (a == b) then "Pass" else ("Fail \n\t value:" ++ (show a) ++ " not equal to expect:" ++ (show b))

tests = [
    ]

runTest (nm, m) = putStrLn (nm ++ " -> " ++ m)

-- unfoldr :: (b -> Maybe (a, b)) -> b -> [a]


main :: IO ()
main = do
    mapM_ runTest tests
