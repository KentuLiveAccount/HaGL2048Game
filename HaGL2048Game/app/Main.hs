module Main (main) where

import Lib

import Graphics.UI.GLUT
import Data.IORef
import Data.List (cycle, unfoldr)
import System.Random
import System.CPUTime

divisor = 24

-- wh for width and height
wh :: GLfloat
wh = ((2::GLfloat) / (fromIntegral grids))

{-
-- -1 - -0.5 - 0 - 0.5 - 1
--    -0.75 -0.25 0.25 0.75
   increments = 1/2n
   centers = increments + 2 increments * (i)  (where 0 <=i < n)
   
-}


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


anyMotion :: [LinearMotion] -> Bool
anyMotion lms = any f lms
  where
    f :: LinearMotion -> Bool
    f (LM (cx, cy) (dx, dy) vc vd) = (abs $ dx - cx) > eps || (abs $ dy - cy) > eps || vc /= vd
    eps = (1::GLfloat) / divisor

animateLm :: LinearMotion -> LinearMotion
animateLm (LM (cx, cy) (dx, dy) vs vd) = (LM (cx', cy') (dx, dy) vs' vd)
  where
    epsilon = (1::GLfloat) / divisor
    cx' = advance cx dx
    cy' = advance cy dy
    vs' = if (abs $ cx' - dx) < epsilon && (abs $ cy' - dy) < epsilon then vd else vs
    advance :: GLfloat -> GLfloat -> GLfloat
    advance cur dest = if (abs $ dest - cur) < epsilon then dest else (cur + epsilon * 2 * (sign $ dest - cur))


data AppState = AS {
  tiles :: [Tile],
  motions :: [LinearMotion], -- Left (cur), (to) | Right (cur)
  interval :: Timeout, -- time out intervals
  rands :: [Int]
  } deriving (Show)

initialAppState :: [Int] -> AppState
initialAppState rs = AS tiles (initialLMs tiles) 0 rs
  where
--    tiles = [TL (0, 3)]
    tiles = [TL (0, 3) 1, TL (0, 2) 1, TL (1, 2) 1, TL (1, 1) 1]
    initialLMs :: [Tile] -> [LinearMotion]
    initialLMs  = map (stillMotion)

onMove :: (Int, Int) -> AppState -> AppState
onMove mv (AS tls lms _ rs) = (AS tls' lms' (if f then 16 else 0) rs')
    where
      (tls', lms', f, rs') = moveTiles rs mv tls

onTime :: AppState -> AppState
onTime (AS tls lms to rs) = (AS tls lms' to' rs)
  where
    lms' = map (animateLm) lms
    to' = if (anyMotion lms) then to else 0

winWidth :: GLsizei
winWidth = 600

genRandom :: IO [Int]
genRandom = let
    randStep :: (Word, Word) -> StdGen -> Maybe (Word, StdGen)
    randStep range gen = Just $ randomR range gen
  in do
    tim <- getCPUTime
    return $ map (fromIntegral) $ take 97 $ unfoldr (randStep (0, fromIntegral $ grids * grids - 1)) (mkStdGen (fromIntegral $ tim `mod` 7919))

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  rs <- genRandom
  stateRef <- newIORef $ initialAppState (cycle rs)
  initialDisplayMode $= [DoubleBuffered]
  _window <- createWindow "Hello World"
  displayCallback $= (display stateRef)
  addTimerCallback 16 (timerProc stateRef)
  keyboardMouseCallback $= Just (keyboardMouse stateRef)
  windowSize $= (Size winWidth winWidth)
  mainLoop

{- 
(0, 0) : middle of window
(-1, 1): upper left corner
(1, 1) : upper right corner
-}

renderLabel :: LinearMotion -> IO ()
renderLabel (LM (x, y) (_, _) v _) = do
    hdx <- stringWidth TimesRoman24 (show v) >>= \dx -> return $ (fromIntegral dx) / (fromIntegral winWidth)
    hdy <- fontHeight TimesRoman24 >>= \dy -> return $ dy / (fromIntegral winWidth)
    --putStrLn $ "dx, dy" ++ (show dx) ++ ", " ++ (show dy)
    rasterPos (Vertex3 (x - hdx) (y - hdy) (1 :: GLfloat))
    --rasterPos (Vertex3 x y (1 :: GLfloat))
    renderString TimesRoman24 (show v)

display :: IORef AppState -> DisplayCallback
display ior = do 
  lms <- readIORef ior >>= return . motions
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
    color (Color3 1 1 (1 :: GLfloat))
    mapM_ renderLabel lms
    )
  swapBuffers

updateTimer :: IORef AppState -> IO ()
updateTimer ior = do
    timeout <- readIORef ior >>= return . interval
    if (timeout > 0) then addTimerCallback timeout $ timerProc ior else return ()

timerProc :: IORef AppState -> IO ()
timerProc ior = do
    updateTimer ior
    postRedisplay Nothing
    modifyIORef ior onTime

-- ($~!) :: MonadIO m => t -> (a -> b) -> m () 
keyboardMouse :: IORef AppState -> KeyboardMouseCallback
keyboardMouse ior key Down _ _ = case key of
  (SpecialKey KeyLeft ) -> ior $~! onMove (-1,  0) >> updateTimer ior
  (SpecialKey KeyRight) -> ior $~! onMove ( 1,  0) >> updateTimer ior
  (SpecialKey KeyUp ) -> ior $~! onMove ( 0,  1) >> updateTimer ior
  (SpecialKey KeyDown) -> ior $~! onMove ( 0, -1) >> updateTimer ior
  _ -> return ()
keyboardMouse _ _ _ _ _ = return ()
