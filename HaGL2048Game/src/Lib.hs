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

moveDir :: (Int, Int) -> [Tile] -> [Tile]
moveDir dir tls = concat $ zipWith (\x y -> map (fn dir x) y) (dirNum dir) (groupBy (samePos) tls)
    where
    fn :: (Int, Int) -> (Int, Int) -> Tile -> Tile
    fn (0, 0)  _       (TL (x, y) v) = TL ( x,  y) v
    fn (_, 0) (xn,  _) (TL (x, y) v) = TL (xn,  y) v
    fn (0, _) ( _, yn) (TL (x, y) v) = TL ( x, yn) v

dirNum :: (Int, Int) -> [(Int, Int)]
dirNum (1, 0) = [(x, 0) | x <- (reverse [0..(grids - 1)])]
dirNum (-1, 0) = [(x, 0) | x <- [0..(grids - 1)]]
dirNum (0, 1) = [(0, y) | y <- (reverse [0..(grids - 1)])]
dirNum (0, -1) = [(0, y) | y <- [0..(grids - 1)]]

consolidateTiles :: [Tile] -> [Tile]
consolidateTiles [] = []
consolidateTiles [t] = [t]
consolidateTiles (t1:t2:ts) = if (v1 == v2 && (abs $ x1 - x2) + (abs $ y1 - y2) == 1) then
         ((TL (x1, y1) (v1 + v1)) : (TL (x1, y1) (v1 + v1)) : (consolidateTiles ts)) 
         else (t1 : (consolidateTiles $ t2:ts))
    where
        (x1, y1) =  pos t1
        v1 = val t1
        (x2, y2) = pos t2
        v2 = val t2

filterDuplicate :: [Tile] -> [Tile]
filterDuplicate [] = []
filterDuplicate [a] = [a]
filterDuplicate (a1:a2:as)
    | (pos a1) == (pos a2) = a2 : (filterDuplicate as)
    | otherwise = a1 : (filterDuplicate (a2:as))

moveTiles :: (Int, Int) -> [Tile] -> ([Tile], [LinearMotion], Bool)
moveTiles dir tiles = (
    if (noNew) then tiles'' else (newTile : tiles'') ,
    if (noNew) then lms else (stillMotion newTile : lms), changed)
  where
    --tilesSorted = concatMap consolidateTiles $ splitDir dir $ sortBy (sortDir dir) tiles
    tilesSorted = sortBy (sortDir dir) tiles
    tiles' = concat $ map (consolidateTiles . moveDir dir) $ splitDir dir tilesSorted
    changed = tilesSorted /= tiles'
    tiles'' = filterDuplicate tiles'
    newTile = (TL (0, 3) 1)
    noNew = any (\x -> (pos x) == (pos newTile)) tiles''
    lms = zipWith (enMotion) tilesSorted tiles'