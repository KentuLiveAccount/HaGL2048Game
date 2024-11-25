module Main (main) where

import Lib

import Graphics.UI.GLUT
import Data.IORef

divisor = 24

toRect :: (GLfloat,GLfloat,GLfloat) -> [(GLfloat,GLfloat,GLfloat)]
toRect (x1, y1, z1) = [(x - 0.05, y - 0.05, z), (x +  0.05, y - 0.05, z), (x + 0.05, y + 0.05, z), (x - 0.05, y + 0.05, z)]
  where
    x = x1 -- * 0.9
    y = y1 -- * 0.9
    z = z1 -- * 0.9

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
    foo m cur dest eps = if (((abs m) < epsilon) || (abs $ dest - cur) >= eps) then dest else (if (m < 0) then -1 else 1)

data AppState = AS {
  motions :: [LinearMotion], -- Left (cur), (to) | Right (cur)
  interval :: Timeout -- time out intervals
  }

onMotion :: ([LinearMotion] -> [LinearMotion]) -> AppState -> AppState
onMotion f as = as {motions = (f $ motions as)}


initialAppState :: AppState
initialAppState = (AS [LM (-1, 1) (-1, 1)] 16)

nextAppState :: AppState -> AppState
nextAppState (AS lms to) = (AS lms' to)
  where
    lms' = map (advanceLm) lms
    increment = 1.0 / divisor

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
    -- scale 0.8 0.8 (0.8::GLfloat)
    scale 0.9 0.9 (0.9::GLfloat)
    -- renderPrimitive :: PrimitiveMode -> IO a -> IO a
    renderPrimitive Quads $ do
      color3f 0 1 0
      mapM_ (\(x, y, z) ->  (vertex $ Vertex3 x y z)) $ concat [toRect (0::GLfloat, 0, 0), toRect (1::GLfloat, 1, 1), toRect (-1::GLfloat, 1, 1)]
    renderPrimitive Quads $ do
      color3f 0 0 1
      mapM_ (\(x, y, z) ->  (vertex $ Vertex3 x y z)) $ concatMap (toRect.(\(x, y) -> (x, y, 0)).cur) lms
    )
  swapBuffers

timerProc :: IORef AppState -> IO ()
timerProc ior = do
    (AS _ timeout) <- readIORef ior
    addTimerCallback timeout $ timerProc ior
    modifyIORef ior nextAppState
    postRedisplay Nothing

-- ($~!) :: MonadIO m => t -> (a -> b) -> m () 
keyboardMouse :: IORef AppState -> KeyboardMouseCallback
keyboardMouse ior key Down _ _ = case key of
  (SpecialKey KeyLeft ) -> ior $~! onMotion (map (moveLm (-1,  0)))
  (SpecialKey KeyRight) -> ior $~! onMotion (map (moveLm ( 1,  0)))
  (SpecialKey KeyUp   ) -> ior $~! onMotion (map (moveLm ( 0,  1)))
  (SpecialKey KeyDown ) -> ior $~! onMotion (map (moveLm ( 0, -1)))
  _ -> return ()
keyboardMouse _ _ _ _ _ = return ()
