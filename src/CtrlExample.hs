{-# LANGUAGE DataKinds, FlexibleContexts, TypeOperators #-}

module Main where



import Control.Monad.State
import Control.Wire hiding (unless)

import FRP.Netwire.Input
import FRP.Netwire.Input.GLFW

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL

import Data.Vinyl hiding ((<+>))
import qualified Data.Vinyl as Vy ((<+>))
import Linear (V2(..), V3(..))

import qualified Control.Monad as Monad (when)
import Data.Maybe (isNothing)
import System.Directory (getCurrentDirectory, setCurrentDirectory)

import CursorProgram (CursorCircleStyle, cursorCircle)



-- Some type synonyms to keep our code clean
type GameMonad = GLFWInputT IO
type GameSession = Session IO (Timed Float ())

-- This wire takes a Vinyl record whose fields have those of the passed in rendering
-- function as a subset and renders according to that function. In reality, this wire
-- doesn't need to be a wire, and could just be a monad to render, but this way we can
-- render what we need without having to go through the plumbing of our main game loop
renderWire :: (Monoid e, renderdata <: i) =>
	(FieldRec renderdata -> IO ())
	-> Wire s e GameMonad (FieldRec i) ()
renderWire rfn = mkGen_ $ \appInfo -> lift $ rfn (rcast appInfo) >> (return $ Right ())

-- This wire produces the position of the circle. It simply follows the mouse cursor
-- but negates the y-value. The origin of the mouse coordinates are in the top left
-- corner of the screen with the y-axis pointing down while the y-axis for rendering
-- points up.
posWire :: Monoid e => Wire s e GameMonad a (V2 GL.GLfloat)
posWire = mouseCursor >>> (second $ arr negate) >>> (arr $ uncurry V2)

-- This wire produces color for the circle. If the R, G, or B keys are pressed,
-- then the circle will turn red, green, or blue, respectively. Otherwise,
-- the red and green channels of the circle pulsate until the F key is pressed,
-- at w/c point it will turn into a cyan circle.
colorWire :: (HasTime t s, Monoid e) => Wire s e GameMonad a (V3 GL.GLfloat)
colorWire =
  -- Key debounced means that it will only flash blue for one frame
  (keyDebounced GLFW.Key'B >>> (pure $ V3 0 0 1)) <|>

  -- Key pressed means that it will remain this color
  (keyPressed GLFW.Key'R >>> (pure $ V3 1 0 0)) <|>
  (keyPressed GLFW.Key'G >>> (pure $ V3 0 1 0)) <|>

  -- Key ever been pressed means it will default to this color
  ( (mkId &&& ((keyPressed GLFW.Key'C >>> (pure $ pure $ V3 0 1 1) >>> now) <|> never)) >>>
  -- Otherwise, pulsate based on the amount of time passed
    ( rSwitch $ timeF >>> (arr (cos &&& sin)) >>> (arr $ \(x, y) -> V3 x y 1) ) )

-- Wire that behaves like the identity wire until Q is pressed, then inhibits forever.
-- We can compose our main gameWire with this wire to simply quit the program when q is pressed
quitWire :: Monoid e => Wire s e GameMonad a a
quitWire = (mkId &&& eventWire) >>> (rSwitch mkId)
  where
    eventWire :: Monoid e => Wire s e GameMonad a (Event (Wire s e m a a))
    eventWire = (keyPressed GLFW.Key'Q >>> pure mkEmpty >>> now) <|> never

-- type CursorCircleStyle = '[ '("offset", V2 GL.GLfloat), '("color", V3 GL.GLfloat) ]

-- This is our main game wire, it feeds the position and color into the rendering loop
-- and finally quits if q is pressed.
gameWire :: (HasTime t s, Monoid e) =>
	(FieldRec CursorCircleStyle -> IO ())
	-> Wire s e GameMonad a ()
gameWire rfn = quitWire
	>>> posWire &&& colorWire
	>>> arr (\(pos, color) -> SField =: pos Vy.<+> SField =: color :: FieldRec CursorCircleStyle)
	>>> (renderWire rfn)

run :: GLFW.Window -> GLFWInputControl -> IO ()
run win ictl = do
  -- initialize the input
  ipt <- getInput ictl

  -- Binding this loads the shaders & compiles the shader program,
  -- & can be done per-frame or on scene change just as well.
  -- graphVerts <- fmap (map (map (*105)) Prelude.. read) $ readFile ("graphs"</>"GraphVerts.txt")
  -- graphEdgeInds <- fmap read $ readFile ("graphs"</>"GraphEdgeInds.txt")
  -- let graphEdgeInds = [[0,1],[2,3],[0,2]]
  -- cursCirc <- bestDrawing $ comp1Dvref =: graphVerts Vy.<+> comp1Deref =: graphEdgeInds
  cursCirc <- cursorCircle {- Can switch to (cursorCircle2) -}

  runGame ipt (countSession_ 0.02) (gameWire cursCirc)

  where

    -- The game loop takes the current input state, the time session and
    -- our main game wire, and simply steps the wire until it inhibits.
    runGame ipt sess w = do

      -- Before rendering clear the framebuffer
      GL.clearColor GL.$= GL.Color4 0.0 0.0 0.0 1
      GL.clear [GL.ColorBuffer]

      -- Poll the current input
      ipt' <- pollGLFW ipt ictl

      -- Figure out our next timestep
      (timeState, sess') <- stepSession sess

      -- Since the GameMonad is a 'StateT GLFWInputState m', in order to
      -- step the wires, we have to extract the value from our wire. That means
      -- that when we runStateT, we will get the results of our wire and a new
      -- state (for example if the wire debounced any keys). This is what we pass
      -- back to GLFW.
      --  renderPrg :: IO ((Either e (), Wire s e GameMonad a ()), GLFWInputState)
      let renderPrg = runGLFWInputT (stepWire w timeState (Right undefined)) ipt'

      -- Now run the actual IO program to extract the values from it.
      ((result, w'), ipt'') <- renderPrg

      -- End of frame cleanup
      GL.flush
      GLFW.swapBuffers win

      -- Our quit condition is if the OS asked us to quit, or the wire inhibits
      -- (i.e. someone hit the Q key)
      case result of
        Left () -> return ()
        Right () -> do
          q <- GLFW.windowShouldClose win
          unless q $ runGame ipt'' sess' w'

initGL :: String -> Int -> Int -> IO (GLFW.Window, GLFWInputControl)
initGL windowTitle width height = do
	currDir <- getCurrentDirectory

	r <- GLFW.init
	Monad.when (not r) (error "Error initializing GLFW!")

	-- GLSL version determined by GL version
	GLFW.windowHint $ GLFW.WindowHint'ClientAPI GLFW.ClientAPI'OpenGL
	GLFW.windowHint $ GLFW.WindowHint'OpenGLForwardCompat True
	GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
	GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
	GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 2

	m@(~(Just w)) <- GLFW.createWindow width height windowTitle Nothing Nothing
	Monad.when (isNothing m) (error "Couldn't create window!")

	GLFW.makeContextCurrent m

	-- Hack for retina displays
	(szx, szy) <- GLFW.getFramebufferSize w
	GL.viewport GL.$= (GL.Position 0 0, GL.Size (fromIntegral szx) (fromIntegral szy))

	setCurrentDirectory currDir

	mkInputControl w >>= (\x -> return (w, x))

main :: IO ()
main = initGL "Netwire Input Demo" 500 500 >>= uncurry run
