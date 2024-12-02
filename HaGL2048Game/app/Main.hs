module Main (main) where

import Lib

import Graphics.UI.GLUT
import Data.IORef
import Data.List (sortBy)

divisor = 24

grids :: Int
grids = 4

wh :: GLfloat
wh = ((2::GLfloat) / (fromIntegral grids))

{-
-- -1 - -0.5 - 0 - 0.5 - 1
--    -0.75 -0.25 0.25 0.75
   increments = 1/2n
   centers = increments + 2 increments * (i)  (where 0 <=i < n)
   
-}

gridPosToPos :: Int -> GLfloat
gridPosToPos v = -1 + increments + 2 * increments * (fromIntegral v)
  where
    increments :: GLfloat
    increments = 2 / (2 * (fromIntegral grids))


sign :: (Ord a, Num a) => a -> a
sign v = if (v < 0) then -1 else 1

toRect :: (GLfloat,GLfloat,GLfloat) -> [(GLfloat,GLfloat,GLfloat)]
toRect (x1, y1, z1) = [(x - hwh, y - hwh, z), (x +  hwh, y - hwh, z), (x + hwh, y + hwh, z), (x - hwh, y + hwh, z)]
  where
    x = x1 -- * 0.9
    y = y1 -- * 0.9
    z = z1 -- * 0.9
    hwh = (wh / 2) * 0.95

color3f :: GLfloat -> GLfloat -> GLfloat -> IO ()
color3f r g b = color $ Color3 r g b

data Tile = TL {pos :: (Int, Int)} deriving (Eq)

data LinearMotion = LM {
    cur :: (GLfloat, GLfloat),
    dest :: (GLfloat, GLfloat)
}

stillMotion :: Tile -> LinearMotion
stillMotion (TL (x, y)) =LM (gridPosToPos x, gridPosToPos y) (gridPosToPos x, gridPosToPos y)

enMotion :: Tile -> Tile -> LinearMotion
enMotion (TL (xs, ys)) (TL (xd, yd)) =LM (gridPosToPos xs, gridPosToPos ys) (gridPosToPos xd, gridPosToPos yd)

anyMotion :: [LinearMotion] -> Bool
anyMotion lms = any f lms
  where
    f :: LinearMotion -> Bool
    f (LM (cx, cy) (dx, dy)) = (abs $ dx - cx) > eps || (abs $ dy - cy) > eps
    eps = (2::GLfloat) / divisor

animateLm :: LinearMotion -> LinearMotion
animateLm (LM (cx, cy) (dx, dy)) = (LM (cx', cy') (dx, dy))
  where
    epsilon = (2::GLfloat) / divisor
    cx' = advance cx dx epsilon
    cy' = advance cy dy epsilon
    advance :: GLfloat -> GLfloat -> GLfloat -> GLfloat
    advance cur dest eps = if (abs $ dest - cur) < (eps / 2) then dest else (cur + eps * (sign $ dest - cur))

moveDestLm :: (Int, Int) -> LinearMotion -> LinearMotion
moveDestLm (xm, ym) (LM (cx, cy) (dx, dy)) = (LM (cx, cy) (dx', dy'))
  where
    dx' = newValue xm cx dx
    dy' = newValue ym cy dy
    newValue :: Int -> GLfloat -> GLfloat -> GLfloat
    newValue m cur dest = if ((abs m) == 0) then dest else (if ((sign m) < 0) then (gridPosToPos 0) else (gridPosToPos 3))

moveTiles :: (Int, Int) -> [Tile] -> ([Tile], [LinearMotion], Bool)
moveTiles dir@(xm, ym) tiles = (tiles', zipWith (enMotion) tilesSorted tiles', tilesSorted == tiles')
  where
    tilesSorted = sortBy (sortDir dir) tiles
    tiles' = concatMap (moveDir dir) $ splitDir dir tilesSorted

    sortDir :: (Int, Int) -> Tile -> Tile -> Ordering
    sortDir (-1,  0) (TL (xa, ya)) (TL (xb, yb)) = if (xa == xb) then (compare ya yb) else (compare xa xb)
    sortDir ( 1,  0) (TL (xa, ya)) (TL (xb, yb)) = if (xa == xb) then (compare ya yb) else (compare xb xa)
    sortDir ( 0, -1) (TL (xa, ya)) (TL (xb, yb)) = if (ya == yb) then (compare xa xb) else (compare ya yb)
    sortDir ( 0,  1) (TL (xa, ya)) (TL (xb, yb)) = if (ya == yb) then (compare xa xb) else (compare yb ya)
    sortDir (_, _) _ _ = EQ
   
    splitDir :: (Int, Int) -> [Tile] -> [[Tile]]
    splitDir _ [] = [[]]
    splitDir _ [t] = [[t]]
    splitDir dir (t:tls) = (t:ms): (splitDir dir us)
      where
        (ms, us) = span (predDir dir t) tls

    predDir :: (Int, Int) -> Tile -> Tile -> Bool
    predDir (-1,  0) (TL (x1, _)) (TL (x2, _)) = x1 == x2
    predDir ( 1,  0) (TL (x1, _)) (TL (x2, _)) = x1 == x2
    predDir ( 0, -1) (TL (_, y1)) (TL (_, y2)) = y1 == y2
    predDir ( 0,  1) (TL (_, y1)) (TL (_, y2)) = y1 == y2

    moveDir :: (Int, Int) -> [Tile] -> [Tile]
    moveDir dir tls = zipWith (fn) tls (dirNum dir)
      where
        fn :: Tile -> (Int, Int) -> Tile
        fn (TL (x, y)) (xn, 0) = TL (xn, y)
        fn (TL (x, y)) (0, yn) = TL (x, yn)
        fn (TL (x, y)) (0, 0) = TL (x, y)

    dirNum :: (Int, Int) -> [(Int, Int)]
    dirNum (1, 0) = [(x, 0) | x <- [(grids - 1)..0]]
    dirNum (-1, 0) = [(x, 0) | x <- [0..(grids - 1)]]
    dirNum (0, 1) = [(0, y) | y <- [(grids - 1)..0]]
    dirNum (0, -1) = [(0, y) | y <- [0..(grids - 1)]]

data AppState = AS {
  tiles :: [Tile],
  motions :: [LinearMotion], -- Left (cur), (to) | Right (cur)
  interval :: Timeout -- time out intervals
  }

initialAppState :: AppState
initialAppState = AS tiles (initialLMs tiles) 0
  where
    tiles = [TL (0, 3), TL (0, 2)]
    initialLMs :: [Tile] -> [LinearMotion]
    initialLMs  = map (stillMotion)

onMotion :: ([LinearMotion] -> [LinearMotion]) -> AppState -> AppState
onMotion f (AS tls lms to) = AS tls lms' to'
  where
    lms' = if (to == 0) then (f lms) else lms
    to' = if (anyMotion lms') then 16 else 0

onTime :: AppState -> AppState
onTime (AS tls lms to) = (AS tls lms' to')
  where
    lms' = map (animateLm) lms
    increment = 1.0 / divisor
    to' = if (anyMotion lms') then to else 0

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  stateRef <- newIORef initialAppState
  initialDisplayMode $= [DoubleBuffered]
  _window <- createWindow "Hello World"
  displayCallback $= (display stateRef)
  addTimerCallback 16 (timerProc stateRef)
  keyboardMouseCallback $= Just (keyboardMouse stateRef)
  windowSize $= (Size 600 600)
  mainLoop

{- 
(0, 0) : middle of window
(-1, 1): upper left corner
(1, 1) : upper right corner
-}

display :: IORef AppState -> DisplayCallback
display ior = do 
  (AS _ lms _) <- readIORef ior
  clear [ColorBuffer] -- ColorBuffer :: ClearBuffer
  preservingMatrix ( do
    --scale 0.8 0.8 (0.8::GLfloat)
    scale 0.9 0.9 (0.9::GLfloat)
    -- renderPrimitive :: PrimitiveMode -> IO a -> IO a
    renderPrimitive Quads $ do
      color3f 0 1 0
      mapM_ (\(x, y, z) ->  (vertex $ Vertex3 x y z)) $ concat [toRect (-0.75, 0.75, 1), toRect (-0.25::GLfloat, 0.75, 1), toRect (0.25::GLfloat, 0.75, 1), toRect (0.75::GLfloat, 0.75, 1)]
    renderPrimitive Quads $ do
      color3f 1 0 0
      mapM_ (\(x, y, z) ->  (vertex $ Vertex3 x y z)) $ concat [toRect (-0.25::GLfloat, 0.75, 1), toRect (0.75::GLfloat, 0.75, 1)]
    renderPrimitive Quads $ do
      color3f 0 0 1
      mapM_ (\(x, y, z) ->  (vertex $ Vertex3 x y z)) $ concatMap (toRect.(\(x, y) -> (x, y, 0)).cur) lms
    )
  swapBuffers

updateTimer :: IORef AppState -> IO ()
updateTimer ior = do
    (AS _ _ timeout) <- readIORef ior
    if (timeout > 0) then addTimerCallback timeout $ timerProc ior else return ()


timerProc :: IORef AppState -> IO ()
timerProc ior = do
    updateTimer ior
    postRedisplay Nothing
    modifyIORef ior onTime

-- ($~!) :: MonadIO m => t -> (a -> b) -> m () 
keyboardMouse :: IORef AppState -> KeyboardMouseCallback
keyboardMouse ior key Down _ _ = case key of
  (SpecialKey KeyLeft ) -> ior $~! onMotion (map (moveDestLm (-1,  0))) >> updateTimer ior
  (SpecialKey KeyRight) -> ior $~! onMotion (map (moveDestLm ( 1,  0))) >> updateTimer ior
  (SpecialKey KeyUp   ) -> ior $~! onMotion (map (moveDestLm ( 0,  1))) >> updateTimer ior
  (SpecialKey KeyDown ) -> ior $~! onMotion (map (moveDestLm ( 0, -1))) >> updateTimer ior
  _ -> return ()
keyboardMouse _ _ _ _ _ = return ()
