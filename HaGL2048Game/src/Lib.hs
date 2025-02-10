module Lib (
    grids,
    gridPosToPos,
    moveTiles,
    stillMotion,
    enMotion,
    predDir,
    splitDir,
    sortDir,
    moveDir,
    dirNum,
    Tile (..),
    LinearMotion (..)
    ) where

import Graphics.UI.GLUT (GLfloat)
import Data.List (sortBy, groupBy)

data Tile = TL {pos :: (Int, Int), val :: Int} deriving (Eq, Show)

data LinearMotion = LM {
    cur :: (GLfloat, GLfloat),
    dest :: (GLfloat, GLfloat),
    valCur :: Int,
    valDest :: Int
} deriving (Eq, Show)

grids :: Int
grids = 4


gridPosToPos :: Int -> GLfloat
gridPosToPos v = -1 + increments + 2 * increments * (fromIntegral v)
  where
    increments :: GLfloat
    increments = 2 / (2 * (fromIntegral grids))


stillMotion :: Tile -> LinearMotion
stillMotion (TL (x, y) v) =LM (gridPosToPos x, gridPosToPos y) (gridPosToPos x, gridPosToPos y) v v

enMotion :: Tile -> Tile -> LinearMotion
enMotion (TL (xs, ys) vs) (TL (xd, yd) vd) =LM (gridPosToPos xs, gridPosToPos ys) (gridPosToPos xd, gridPosToPos yd) vs vd

predDir :: (Int, Int) -> Tile -> Tile -> Bool
predDir ( 0,  0) (TL (x1, _) _) (TL (x2, _) _) = True
predDir ( 0,  _) (TL (x1, _) _) (TL (x2, _) _) = x1 == x2
predDir ( _,  0) (TL (_, y1) _) (TL (_, y2) _) = y1 == y2

foo :: (a -> a -> Maybe [a]) -> [a] -> [a]
foo _ [] = []
foo _ [a] = [a]
foo f (a1:a2:as) = case (f a1 a2) of
    Just aa   -> aa ++ (foo f as)
    otherwise -> a1 : (foo f (a2:as))

splitDir :: (Int, Int) -> [Tile] -> [[Tile]]
splitDir _ [] = []
splitDir _ [t] = [[t]]
splitDir dir (t:tls) = (t:ms): (splitDir dir us)
    where
    (ms, us) = span (predDir dir t) tls

sortDir :: (Int, Int) -> Tile -> Tile -> Ordering
sortDir ( 0, -1) (TL (xa, ya) _) (TL (xb, yb) _) = if (xa == xb) then (compare ya yb) else (compare xa xb)
sortDir ( 0,  1) (TL (xa, ya) _) (TL (xb, yb) _) = if (xa == xb) then (compare yb ya) else (compare xa xb)
sortDir (-1,  0) (TL (xa, ya) _) (TL (xb, yb) _) = if (ya == yb) then (compare xa xb) else (compare yb ya)
sortDir ( 1,  0) (TL (xa, ya) _) (TL (xb, yb) _) = if (ya == yb) then (compare xb xa) else (compare ya yb)
sortDir (_, _) _ _ = EQ

samePos :: Tile -> Tile -> Bool
samePos t1 t2 = (pos t1) == (pos t2)

sameVal :: Tile -> Tile -> Bool
sameVal t1 t2 = (val t1) == (val t2)

moveDir :: (Int, Int) -> [Tile] -> [Tile]
moveDir dir tls = concat $ zipWith (\x y -> map (fn dir x) y) (dirNum dir) (groupBy (samePos) tls)
    where
    fn :: (Int, Int) -> (Int, Int) -> Tile -> Tile
    fn (0, 0)  _       (TL (x, y) v) = TL ( x,  y) v
    fn (_, 0) (xn,  _) (TL (x, y) v) = TL (xn,  y) v
    fn (0, _) ( _, yn) (TL (x, y) v) = TL ( x, yn) v

dirNum :: (Int, Int) -> [(Int, Int)]
dirNum ( 1,  0) = [(x, 0) | x <- (reverse [0..(grids - 1)])]
dirNum (-1,  0) = [(x, 0) | x <- [0..(grids - 1)]]
dirNum ( 0,  1) = [(0, y) | y <- (reverse [0..(grids - 1)])]
dirNum ( 0, -1) = [(0, y) | y <- [0..(grids - 1)]]

consolidateTiles :: [Tile] -> [Tile]
consolidateTiles = foo (\t1 t2 -> if (sameVal t1 t2) then Just [doubleVal t1, doubleVal t1] else Nothing)
    where
        doubleVal (TL pos v) = TL pos (v+v)

filterDuplicate :: [Tile] -> [Tile]
filterDuplicate = foo (\a1 a2 -> if (pos a1) == (pos a2) then Just [a2] else Nothing)

findNewPos :: [Tile] -> [Int] -> ((Int, Int), [Int])
findNewPos tiles ints = ((i `div` grids, i `mod` grids), is)
    where
        (i:is) = dropWhile (reject) ints
        poss = map ((\(x, y) -> x * grids + y ) . pos) tiles
        reject :: Int -> Bool
        reject i = any (== (i `mod` (grids * grids))) poss

moveTiles :: [Int] -> (Int, Int) -> [Tile] -> ([Tile], [LinearMotion], Bool, [Int])
moveTiles news dir tiles = 
    if (noNew) then (tiles'', lms, changed, news') 
    else ((newTile : tiles'') ,(stillMotion newTile : lms), changed, news')
  where
    tilesSorted = sortBy (sortDir dir) tiles
    tiles' = concat $ map (moveDir dir . consolidateTiles . moveDir dir) $ splitDir dir tilesSorted
    changed = tilesSorted /= tiles'
    tiles'' = filterDuplicate tiles'
    (newPos, news') = findNewPos tiles'' news
    newTile = TL newPos 1
    noNew = (news == []) || (any (samePos newTile) tiles'')
    lms = zipWith (enMotion) tilesSorted tiles'