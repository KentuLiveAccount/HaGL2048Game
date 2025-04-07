module Lib (
    grids,
    gridPosToPos,
    moveTiles,
    stillMotion,
    enMotion,
    compareDir,
    predDir,
    splitDir,
    moveDir,
    dirNum,
    Tile (..),
    LinearMotion (..)
    ) where

import Graphics.UI.GLUT (GLfloat)
import Control.Monad(when)
import Data.Function (on)
import Data.List (sortBy, groupBy)
import Data.Tuple (swap, fst, snd)

orNothing :: Bool -> a -> Maybe a
orNothing True a = Just a
orNothing False _ = Nothing

(?) :: Bool -> a -> a -> a 
(?) True a _ = a
(?) False _ a = a

data Tile = TL {pos :: (Int, Int), val :: Int} deriving (Eq, Show)

swapXY :: Tile -> Tile
swapXY tl = tl {pos = swap $ pos tl}

-- Data.Function.on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
samePos :: Tile -> Tile -> Bool
samePos = on (==) pos

sameVal :: Tile -> Tile -> Bool
sameVal = on (==) val

data LinearMotion = LM {
    cur  :: (GLfloat, GLfloat),
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
stillMotion (TL (x, y) v) = LM (gridPosToPos x, gridPosToPos y) (gridPosToPos x, gridPosToPos y) v v

enMotion :: Tile -> Tile -> LinearMotion
enMotion (TL (xs, ys) vs) (TL (xd, yd) vd) = LM (gridPosToPos xs, gridPosToPos ys) (gridPosToPos xd, gridPosToPos yd) vs vd

predDir :: (Int, Int) -> Tile -> Tile -> Bool
predDir ( 0,  0) = \_ _ -> True
predDir ( 0,  _) = on (==) (fst.pos)
predDir ( _,  0) = on (==) (snd.pos)

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

compareDir :: (Int, Int) -> Tile -> Tile -> Ordering
compareDir ( 0,  1) (TL (xa, ya) _) (TL (xb, yb) _) = (xa == xb) ? (compare yb ya) $ (compare xa xb)
compareDir ( 0, -1) (TL (xa, ya) _) (TL (xb, yb) _) = (xa == xb) ? (compare ya yb) $ (compare xa xb)
compareDir ( 1,  0) t1 t2 = compareDir (0,  1) (swapXY t1) (swapXY t2)
compareDir (-1,  0) t1 t2 = compareDir (0, -1) (swapXY t1) (swapXY t2)
compareDir (_, _) _ _ = EQ

moveDir :: (Int, Int) -> [Tile] -> [Tile]
moveDir dir tls = concat $ zipWith (\x y -> map (fn dir x) y) (dirNum dir) (groupBy (samePos) tls)
    where
    fn :: (Int, Int) -> (Int, Int) -> Tile -> Tile
    fn (0, 0)  _       (TL (x, y) v) = TL ( x,  y) v
    fn (_, 0) (xn,  _) (TL (x, y) v) = TL (xn,  y) v
    fn (0, _) ( _, yn) (TL (x, y) v) = TL ( x, yn) v

dirNum :: (Int, Int) -> [(Int, Int)]
dirNum ( 1,  0) = [(x, 0) | x <- (reverse [0..(grids - 1)])]
dirNum (-1,  0) = reverse  $ dirNum (1, 0)
dirNum ( 0,  1) = map swap $ dirNum (1, 0)
dirNum ( 0, -1) = reverse  $ dirNum (0, 1)

consolidateTiles :: [Tile] -> [Tile]
consolidateTiles = foo (\t1 t2 -> orNothing (sameVal t1 t2) [doubleVal t1, doubleVal t1])
    where
        doubleVal tl = tl {val = 2 * (val tl)}

filterDuplicate :: [Tile] -> [Tile]
filterDuplicate = foo (\a1 a2 -> orNothing (samePos a1 a2) [a2])

findNewPos :: [Tile] -> [Int] -> ((Int, Int), [Int])
findNewPos tiles ints = ((i `div` grids, i `mod` grids), is)
    where
        (i:is) = dropWhile (reject) ints
        poss = map ((\(x, y) -> x * grids + y ) . pos) tiles
        reject :: Int -> Bool
        reject i = any (== (i `mod` (grids * grids))) poss

moveTiles :: [Int] -> (Int, Int) -> [Tile] -> ([Tile], [LinearMotion], Bool, [Int])
moveTiles news dir tiles = (not changed) ?
    (tiles, map stillMotion tiles, False, news) $
        (noNew) ? (tiles'', lms, changed, news) $
            ((newTile : tiles'') ,(stillMotion newTile : lms), changed, news')
  where
    tilesSorted = sortBy (compareDir dir) tiles
    tiles' = concat $ map (moveDir dir . consolidateTiles . moveDir dir) $ splitDir dir tilesSorted
    changed = tilesSorted /= tiles'
    tiles'' = filterDuplicate tiles'
    (newPos, news') = findNewPos tiles'' news
    newTile = TL newPos 1
    noNew = (not changed) || (null news) || (any (samePos newTile) tiles'')
    lms = zipWith (enMotion) tilesSorted tiles'