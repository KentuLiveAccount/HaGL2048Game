module Main (main) where

import Lib

import Graphics.UI.GLUT
import Data.IORef

divisor = 24

data AppState = AS {step :: GLfloat, interval :: Timeout}

initialAppState :: AppState
initialAppState = (AS 0.0 16)

nextAppState :: AppState -> AppState
nextAppState (AS st to) = (AS st' to)
  where
    st' = if st + increment > 1.0 then 0.0 else st + increment
    increment = 1.0 / divisor

toRect :: (GLfloat,GLfloat,GLfloat) -> [(GLfloat,GLfloat,GLfloat)]
toRect (x, y, z) = [(x, y, z), (x + 0.1, y, z), (x + 0.1, y + 0.1, z), (x, y + 0.1, z)]

myPoints :: GLfloat -> [(GLfloat,GLfloat,GLfloat)]
myPoints dl = concatMap toRect $ map (\(x, y, z) -> (x * 0.9, y * 0.9, z * 0.9)) 
  [ (sin (delta + 2*pi*k/divisor), cos (delta + 2*pi*k/divisor), 0) | k <- [1] ]
    where
        delta = 2 * pi / divisor * dl

color3f :: GLfloat -> GLfloat -> GLfloat -> IO ()
color3f r g b = color $ Color3 r g b

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

display :: IORef AppState -> DisplayCallback
display ior = do 
  (AS dl _) <- readIORef ior
  clear [ColorBuffer] -- ColorBuffer :: ClearBuffer
  preservingMatrix ( do
    scale 0.8 0.8 (0.8::GLfloat)
    -- renderPrimitive :: PrimitiveMode -> IO a -> IO a
    renderPrimitive Quads $ do
      color3f 1 0 0
      mapM_ (\(x, y, z) ->  (vertex $ Vertex3 x y z)) $ myPoints dl)
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
  --(Char ' ') -> ior $~! \(AS st to) -> AS (negate st) to
  (SpecialKey KeyLeft ) -> ior $~! \(AS st to) -> AS st (max 0 (to - 1))
  (SpecialKey KeyRight) -> ior $~! \(AS st to) -> AS st (min 16 (to + 1))
  _ -> return ()
keyboardMouse _ _ _ _ _ = return ()
