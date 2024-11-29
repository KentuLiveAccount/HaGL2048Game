module Main (main) where

import Lib

import Graphics.UI.GLUT
import Data.IORef

divisor = 24

wh :: GLfloat
wh = (2 / 4)

{-
-- -1 - -0.5 - 0 - 0.5 - 1
--    -0.75 -0.25 0.25 0.75
   increments = 1/2n
   centers = increments + 2 increments * (i)  (where 0 <=i < n)
   
-}

toRect :: (GLfloat,GLfloat,GLfloat) -> [(GLfloat,GLfloat,GLfloat)]
toRect (x1, y1, z1) = [(x - hwh, y - hwh, z), (x +  hwh, y - hwh, z), (x + hwh, y + hwh, z), (x - hwh, y + hwh, z)]
  where
    x = x1 -- * 0.9
    y = y1 -- * 0.9
    z = z1 -- * 0.9
    hwh = wh / 2

color3f :: GLfloat -> GLfloat -> GLfloat -> IO ()
color3f r g b = color $ Color3 r g b

data LinearMotion = LM {
    cur :: (GLfloat, GLfloat),
    dest :: (GLfloat, GLfloat)
}

advanceLm :: LinearMotion -> LinearMotion
advanceLm (LM (cx, cy) (dx, dy)) = (LM (cx', cy') (dx, dy))
  where
    epsilon = (2::GLfloat) / divisor
    cx' = foo cx dx epsilon
    cy' = foo cy dy epsilon
    foo :: GLfloat -> GLfloat -> GLfloat -> GLfloat
    foo cur dest eps = if (abs $ dest - cur) < eps then dest else (if (cur < dest) then cur + eps else cur - eps)

moveLm :: (GLfloat, GLfloat) -> LinearMotion -> LinearMotion
moveLm (xm, ym) (LM (cx, cy) (dx, dy)) = (LM (cx, cy) (dx', dy'))
  where
    epsilon = (2::GLfloat) / divisor
    dx' = foo xm cx dx epsilon
    dy' = foo ym cy dy epsilon
    foo :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat
    foo m cur dest eps = if ((abs m) < epsilon) then dest else (if (m < 0) then -1 else 1)

data AppState = AS {
  motions :: [LinearMotion], -- Left (cur), (to) | Right (cur)
  interval :: Timeout -- time out intervals
  }

onMotion :: ([LinearMotion] -> [LinearMotion]) -> AppState -> AppState
onMotion f (AS lms to) = AS lms' to'
  where
    lms' = if (to == 0) then (f lms) else lms
    to' = if (anyMotion lms') then 16 else 0


anyMotion :: [LinearMotion] -> Bool
anyMotion lms = any f lms
  where
    f :: LinearMotion -> Bool
    f (LM (cx, cy) (dx, dy)) = (abs $ dx - cx) > eps || (abs $ dy - cy) > eps
    eps = (2::GLfloat) / divisor


initialAppState :: AppState
initialAppState = (AS [LM (-1, 1) (1, -1)] 16)

nextAppState :: AppState -> AppState
nextAppState (AS lms to) = (AS lms' to')
  where
    lms' = map (advanceLm) lms
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

foo:: LinearMotion -> [(GLfloat, GLfloat, GLfloat)]
foo = toRect.(\(x, y) -> (x, y, 0)).cur

display :: IORef AppState -> DisplayCallback
display ior = do 
  (AS lms _) <- readIORef ior
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
    (AS _ timeout) <- readIORef ior
    if (timeout > 0) then addTimerCallback timeout $ timerProc ior else return ()


timerProc :: IORef AppState -> IO ()
timerProc ior = do
    modifyIORef ior nextAppState
    updateTimer ior
    postRedisplay Nothing

-- ($~!) :: MonadIO m => t -> (a -> b) -> m () 
keyboardMouse :: IORef AppState -> KeyboardMouseCallback
keyboardMouse ior key Down _ _ = case key of
  (SpecialKey KeyLeft ) -> ior $~! onMotion (map (moveLm (-1,  0))) >> updateTimer ior
  (SpecialKey KeyRight) -> ior $~! onMotion (map (moveLm ( 1,  0))) >> updateTimer ior
  (SpecialKey KeyUp   ) -> ior $~! onMotion (map (moveLm ( 0,  1))) >> updateTimer ior
  (SpecialKey KeyDown ) -> ior $~! onMotion (map (moveLm ( 0, -1))) >> updateTimer ior
  _ -> return ()
keyboardMouse _ _ _ _ _ = return ()
