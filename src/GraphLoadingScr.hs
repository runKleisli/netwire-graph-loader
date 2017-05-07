{-# LANGUAGE DataKinds, FlexibleContexts, TypeOperators #-}

module Main where

--------------------------------------------------------------------------------
import qualified Control.Monad as Monad (when)
import Data.Maybe (isNothing)

import Control.Monad.State
import Control.Wire hiding (unless, until, (.), id)
import qualified Control.Wire as W (until)
import Control.Wire.Unsafe.Event (onEventM)

import FRP.Netwire.Input
import FRP.Netwire.Input.GLFW

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL

-- Loading files
import System.IO
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.DeepSeq (deepseq, NFData)

import Data.Vinyl hiding ((<+>))
import qualified Data.Vinyl as Vy ((<+>))
import Linear (V2(..), V3(..))
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.FilePath ((</>))

import CursorProgram (CursorCircleStyle(..), cursorCircle)

import GraphProgram (ProjInfo2D, CompRec1D, comp1Dvref, comp1Deref, bestDrawing, cursorCircle2)



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

-- Wire that behaves like the identity wire until Q is pressed, then inhibits forever.
-- We can compose our main gameWire with this wire to simply quit the program when q is pressed
quitWire :: Monoid e => Wire s e GameMonad a a
quitWire = (mkId &&& eventWire) >>> (rSwitch mkId)
  where
    eventWire :: Monoid e => Wire s e GameMonad a (Event (Wire s e m a a))
    eventWire = (keyPressed GLFW.Key'Q >>> pure mkEmpty >>> now) <|> never

-- type CursorCircleStyle = '[ '("offset", V2 GL.GLfloat), '("color", V3 GL.GLfloat) ]

initialVal :: (Monoid e, Monad m) => (a -> m b) -> Wire s e m a b
initialVal f = now >>> once >>> onEventM f >>> hold

type MidLoadGraph = '[ '("chunkT_doneF", TChan Bool), '("fedge", TVar [[Int]]) ]

reedyFn :: FilePath -> TVar [[Int]] -> TChan Bool -> IO ()
reedyFn file dest talker = withFile file ReadMode $ \handle -> do
	-- (Char)s are 32-bit words internally, last time I checked.
	hSetBuffering handle $ BlockBuffering (Just $ 4*howmany)
	contents <- hGetContents handle
	loopConsume contents (read contents :: [[Int]]) dest talker
	where
		howmany = 1024 {- Effectively the number of bytes in a chunk (/ 4)d. -}
		-- We use the String rep so we can match words in buffer w/ elems to eval.
		loopConsume :: String -> a -> TVar a -> TChan Bool -> IO ()
		loopConsume "" x sink talk = atomically $ do
			writeTVar sink x	-- (x) has been processed, so output.
			writeTChan talk False -- and tell people about it
		loopConsume xstr x sink talk = (atomically $ writeTChan talk True)
			>> loopConsume (tearOffChunk howmany xstr) x sink talk
		tearOffChunk :: NFData a => Int -> [a] -> [a]
		tearOffChunk = ((uncurry deepseq) .) . splitAt

startLoadingGraph :: (Monoid e) => Wire s e GameMonad a (FieldRec MidLoadGraph)
startLoadingGraph = initialVal $ \_ -> lift $ do
	jacky <- newTChanIO :: IO (TChan Bool)
	francis <- newTVarIO [] :: IO (TVar [[Int]])
	forkIO $ reedyFn ("graphs"</>"GraphEdgeInds.txt") francis jacky
	return $ SField =: jacky Vy.<+> SField =: francis

-- Should become ProjInfo2D
type GraphExtract = '[ '("outedges", [[Int]]) ]

getThatGraph :: (Monoid e, MidLoadGraph <: j)
	=> Wire s e GameMonad (FieldRec j) (FieldRec GraphExtract)
getThatGraph = initialVal $ \datarec -> lift $
	return (rcast datarec :: FieldRec MidLoadGraph)
	>>= (return . getField . rget sink)
	>>= readTVarIO
	>>= (return . (edgesField =:))
	where
		sink = SField :: SField '("fedge", TVar [[Int]])
		edgesField = SField :: SField '("outedges", [[Int]])

beThatGraph :: (Monoid e, GraphExtract <: j)
	=> (FieldRec CursorCircleStyle -> IO ())
	-> Wire s e GameMonad (FieldRec j) ()
beThatGraph rfn = (posWire &&& (pure $ V3 0 1 0)) >>> bindCircStyle >>> renderWire rfn
	where
		bindCircStyle = arr $ \(pos, color) -> SField =: pos Vy.<+> SField =: color :: FieldRec CursorCircleStyle

pretendItsLoading :: (Monoid e, HasTime t s, MidLoadGraph <: j)
	=> (FieldRec CursorCircleStyle -> IO ())
	-> Wire s e GameMonad (FieldRec j) ()
pretendItsLoading rfn =
	-- Emits an event w/ value True when a chunk has loaded, inhibits forever
	-- w/ value False when full file is loaded.

	checkOnIt >>> (became id &&& noLonger id) >>> W.until

	-- Originally, I did this
	-- checkOnIt >>> (became id &&& noLonger id) >>> W.until

	-- But the problem is the event only triggers once.
	-- This suggests the channel is also only read once.

	-- Here the event triggers only once, also.
	-- However, instead of proceeding to the loaded state after a while,
	-- it proceeds to the error "thread blocked indefinitely in an STM transaction"
	-- checkOnIt >>> now >>> (filterE id &&& dropWhileE id) >>> W.until

	-- Perhaps we needed to make it (<|> never)...
	-- But we should also just switch away from this wire once it's used up.
	-- Maybe we just put "now" in in the wrong order.

	-- Maybe we need to make our own stateful arrow, like in (became) and (noLonger).
	-- Or we can just emit events from (checkOnIt).

	-- Keeps track of how many chunks have loaded and represents that visually.
	>>> ( onEventM $ \x -> lift $ print x >> return x )
	>>> tallyChunks >>> hold
	>>> (posWire &&& statusColor) >>> bindCircStyle
	>>> renderWire rfn
	where
		checkOnIt :: (MidLoadGraph <: j) => Wire s e GameMonad (FieldRec j) Bool
		checkOnIt = mkGen_ $ \datarec -> lift $
			return (rcast datarec :: FieldRec MidLoadGraph)
			>>= (return . getField . rget talker)
			>>= (fmap Right . atomically . readTChan)
		talker = SField :: SField '("chunkT_doneF", TChan Bool)
		tallyChunks :: Wire s e m (Event Bool) (Event GL.GLfloat)
		tallyChunks = accumE (\x y -> if y then x+1 else x) 0.0
		statusColor :: (HasTime t s)
			=> Wire s e GameMonad GL.GLfloat (V3 GL.GLfloat)
		statusColor = ( mkId &&& (timeF >>> (arr $ (abs . cos) &&& (abs . sin))) )
			>>> ( arr $ \(x, (y,z)) -> V3 (y/x) (z/x) (1/x) )
		bindCircStyle = arr $ \(pos, color) -> SField =: pos Vy.<+> SField =: color :: FieldRec CursorCircleStyle

splash :: (HasTime t s, Monoid e) =>
	(FieldRec CursorCircleStyle -> IO ())
	-> Wire s e GameMonad a ()
splash dflt =
  -- Once key is pressed it'll start loading & then become a graph.
  ( mkId &&& ((keyPressed GLFW.Key'L >>> pure loadIntoGraph >>> now) <|> never) )
  -- Until then, it'll be this circle dealy.
  >>> rSwitch dfltCircle
  where
    dfltCircle = posWire &&& dfltColor >>> bindCircStyle >>> (renderWire dflt)
    bindCircStyle = arr $ \(pos, color) -> SField =: pos Vy.<+> SField =: color :: FieldRec CursorCircleStyle
    dfltColor = pure $ V3 1 1 1
    -- Use (startLoadingGraph) to get load state, then process it w/ (pretendItsLoading)
    -- until that inhibits, then behave like (getThatGraph >>> beThatGraph)
    loadIntoGraph = startLoadingGraph
      >>> (pretendItsLoading dflt --> (getThatGraph >>> beThatGraph dflt))

-- This is our main game wire, it feeds the position and color into the rendering loop
-- and finally quits if q is pressed.
gameWire :: (HasTime t s, Monoid e)
	=> (FieldRec CursorCircleStyle -> IO ())
	-> Wire s e GameMonad a ()
gameWire rfn = quitWire >>> splash rfn

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
main = initGL "Netwire Input Demo" 1000 1000 >>= uncurry run
