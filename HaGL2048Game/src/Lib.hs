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
import Data.List (sortBy)

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
sortDir (-1,  0) (TL (xa, ya) _) (TL (xb, yb) _) = if (xa == xb) then (compare ya yb) else (compare xa xb)
sortDir ( 1,  0) (TL (xa, ya) _) (TL (xb, yb) _) = if (xa == xb) then (compare ya yb) else (compare xb xa)
sortDir ( 0, -1) (TL (xa, ya) _) (TL (xb, yb) _) = if (ya == yb) then (compare xa xb) else (compare ya yb)
sortDir ( 0,  1) (TL (xa, ya) _) (TL (xb, yb) _) = if (ya == yb) then (compare xa xb) else (compare yb ya)
sortDir (_, _) _ _ = EQ

moveDir :: (Int, Int) -> [Tile] -> [Tile]
moveDir dir tls = zipWith (fn dir) tls (dirNum dir)
    where
    fn :: (Int, Int) -> Tile -> (Int, Int) -> Tile
    fn (0, 0) (TL (x, y) v)  _       = TL ( x,  y) v
    fn (_, 0) (TL (x, y) v) (xn,  _) = TL (xn,  y) v
    fn (0, _) (TL (x, y) v) ( _, yn) = TL ( x, yn) v

dirNum :: (Int, Int) -> [(Int, Int)]
dirNum (1, 0) = [(x, 0) | x <- (reverse [0..(grids - 1)])]
dirNum (-1, 0) = [(x, 0) | x <- [0..(grids - 1)]]
dirNum (0, 1) = [(0, y) | y <- (reverse [0..(grids - 1)])]
dirNum (0, -1) = [(0, y) | y <- [0..(grids - 1)]]

consolidateTiles :: [Tile] -> [Tile]
consolidateTiles [] = []
consolidateTiles [t] = [t]
consolidateTiles (t1:t2:ts) = if (v1 == v2 && (abs $ x1 - x2) + (abs $ y1 - y2) == 1) then
         ((TL (x1, y1) (v1 + v1)) : (consolidateTiles ts)) 
         else (t1 : (consolidateTiles $ t2:ts))
    where
        (x1, y1) =  pos t1
        v1 = val t1
        (x2, y2) = pos t2
        v2 = val t2

moveTiles :: (Int, Int) -> [Tile] -> ([Tile], [LinearMotion], Bool)
moveTiles dir tiles = (tiles', zipWith (enMotion) tilesSorted tiles', tilesSorted /= tiles')
  where
    tilesSorted = sortBy (sortDir dir) tiles
    tiles' = concatMap (moveDir dir) $ map consolidateTiles $ splitDir dir tilesSorted