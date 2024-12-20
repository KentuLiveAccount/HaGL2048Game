module Main (main) where

import Lib

import Graphics.UI.GLUT
import Data.IORef

divisor = 24

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
    vs' = if (abs $ cx' - dx) < epsilon && (abs $ cy' - dx) < epsilon then vd else vs
    advance :: GLfloat -> GLfloat -> GLfloat
    advance cur dest = if (abs $ dest - cur) < epsilon then dest else (cur + epsilon * 2 * (sign $ dest - cur))


data AppState = AS {
  tiles :: [Tile],
  motions :: [LinearMotion], -- Left (cur), (to) | Right (cur)
  interval :: Timeout -- time out intervals
  }

initialAppState :: AppState
initialAppState = AS tiles (initialLMs tiles) 0
  where
--    tiles = [TL (0, 3)]
    tiles = [TL (0, 3) 1, TL (0, 2) 1, TL (1, 2) 1, TL (1, 1) 1]
    initialLMs :: [Tile] -> [LinearMotion]
    initialLMs  = map (stillMotion)

onMove :: (Int, Int) -> AppState -> AppState
onMove mv (AS tls lms _) = (AS tls' lms' (if f then 16 else 0))
    where
      (tls', lms', f) = moveTiles mv tls

onTime :: AppState -> AppState
onTime (AS tls lms to) = (AS tls lms' to')
  where
    lms' = map (animateLm) lms
    to' = if (anyMotion lms') then to else 0

winWidth :: GLsizei
winWidth = 600

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  stateRef <- newIORef initialAppState
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
    color (Color3 1 1 (1 :: GLfloat))
    mapM_ renderLabel lms
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
  (SpecialKey KeyLeft ) -> ior $~! onMove (-1,  0) >> updateTimer ior
  (SpecialKey KeyRight) -> ior $~! onMove ( 1,  0) >> updateTimer ior
  (SpecialKey KeyUp ) -> ior $~! onMove ( 0,  1) >> updateTimer ior
  (SpecialKey KeyDown) -> ior $~! onMove ( 0, -1) >> updateTimer ior
  _ -> return ()
keyboardMouse _ _ _ _ _ = return ()
