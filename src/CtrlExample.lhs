Netwire graph loader - Control example
======

This concerns a control flow behavior required of our FRP engine to build a loading
screen, the means of changing to loading and to loaded states.

This example is derived from Style 2 of netwire-vinylglfw-examples. Before we describe
our problem, let's deal with some preliminaries.

We'll recite the imports and extensions as they're introduced throughout the text,
but for now here module begins, with the same imports as the cursor example.

Extensions for using Vinyl:

> {-# LANGUAGE DataKinds, FlexibleContexts, TypeOperators #-}

> module Main where

Dynamic control expression - FRP:

> import Control.Wire hiding (unless)

User interaction:

> import FRP.Netwire.Input
> import FRP.Netwire.Input.GLFW
> import Control.Monad.State

Graphics:

> import qualified Graphics.UI.GLFW as GLFW
> import qualified Graphics.Rendering.OpenGL as GL

Dataflow:

> import Data.Vinyl hiding ((<+>))
> import qualified Data.Vinyl as Vy ((<+>))

Data:

> import Linear (V2(..), V3(..))

The shader program depicting the cursor & the input it takes:

> import CursorProgram (CursorCircleStyle, cursorCircle)

Configuring program:

> import qualified Control.Monad as Monad (when)
> import Data.Maybe (isNothing)
> import System.Directory (getCurrentDirectory, setCurrentDirectory)



Our FRP library is Netwire, and we'll be confirming the desired control flow behavior
visually by its interactive stimulation. For this we use GLFW with netwire-input.

< import Control.Wire hiding (unless)

< import FRP.Netwire.Input
< import FRP.Netwire.Input.GLFW

< import qualified Graphics.UI.GLFW as GLFW
< import qualified Graphics.Rendering.OpenGL as GL

netwire-input-glfw lets us to access user input reported by GLFW from wires like
(keyPressed) if we make our (Wire) monad one that knows how to refer to the UI state.

< import Control.Monad.State

> type GameMonad = GLFWInputT IO

The following has time:

< type GameSession = Session IO (Timed Float ())



The problem of state preservation
------

FRP arrows are already dynamic, we know from the cursor example that their output can depend on parts of the environment like keypresses.

This is how the color of the cursor is computed in the cursor example:

< -- This wire produces color for the circle. If the R, G, or B keys are pressed,
< -- then the circle will turn red, green, or blue, respectively. Otherwise,
< -- the red and green channels of the circle pulsate
< colorWireOriginal :: (HasTime t s, Monoid e) => Wire s e GameMonad a (V3 GL.GLfloat)
< colorWireOriginal =
<   -- Key debounced means that it will only flash blue for one frame
<   (keyDebounced GLFW.Key'B >>> (pure $ V3 0 0 1)) <|>
< 
<   -- Key pressed means that it will remain this color
<   (keyPressed GLFW.Key'R >>> (pure $ V3 1 0 0)) <|>
<   (keyPressed GLFW.Key'G >>> (pure $ V3 0 1 0)) <|>
< 
<   -- Otherwise, pulsate based on the amount of time passed
<   (timeF >>> (arr (cos &&& sin)) >>> (arr $ \(x, y) -> V3 x y 1))

For a loading screen, though, we need to confirm wires can change to a state persisting after the triggering environment has changed, after the keys are released, because we need to transition from one program state to a loading state to the loaded state where what's loaded is used.

To see we can implement such persistent state changes, it suffices to produce a program where: There is an initial state. When a key is pressed, a different state is entered, and maintained thereafter.

> -- This wire produces color for the circle. If the R, G, or B keys are pressed,
> -- then the circle will turn red, green, or blue, respectively. Otherwise,
> -- the red and green channels of the circle pulsate until the F key is pressed,
> -- at w/c point it will turn into a cyan circle.
> colorWire :: (HasTime t s, Monoid e) => Wire s e GameMonad a (V3 GL.GLfloat)
> colorWire =
>   -- Key debounced means that it will only flash blue for one frame
>   (keyDebounced GLFW.Key'B >>> (pure $ V3 0 0 1)) <|>

>   -- Key pressed means that it will remain this color
>   (keyPressed GLFW.Key'R >>> (pure $ V3 1 0 0)) <|>
>   (keyPressed GLFW.Key'G >>> (pure $ V3 0 1 0)) <|>

>   -- Key ever been pressed means it will default to this color
>   ( (mkId &&& ((keyPressed GLFW.Key'C >>> (pure $ pure $ V3 0 1 1) >>> now) <|> never)) >>>
>   -- Otherwise, pulsate based on the amount of time passed
>     ( rSwitch $ timeF >>> (arr (cos &&& sin)) >>> (arr $ \(x, y) -> V3 x y 1) ) )

The new last choice is almost the same as (quitWire) from the cursor example, which
is used here too, but is used for its producing behavior instead of its inhibiting.

The construct

< (_ >>> now) <|> never

produces the unique first event from a wire producing (not inhibiting), and

< keyPressed GLFW.Key'C

produces when and only when the `C` key is pressed for the current frame, un|modified, so

< keyPressed GLFW.Key'C >>> (pure $ pure $ V3 0 1 1)

does the same, giving the color cyan when producing. The (Wire) input is polymorphic.

< (mkId &&& _) >>> rSwitch _

takes two wires between the same types, and passes the same input into both of them.

The output is taken from the 2nd wire until 1st emits a (Wire ...) by (Event) that
replaces the 2nd wire. Hence, (pure $ pure $ V3 0 1 1) is a wire between the same types
as the argument to (rSwitch), the original color wire's pulsating color state, and
replaces it once the `C` key is pressed.

The 1st Wire can emit further events to switch
the (Wire) again, but composing with the first construct mentioned insures that once
the `C` key has been pressed, the color of the switching wire is cyan forever.

The rest of the code needed for this demonstration is the same as in the cursor example.

There's further discussion of state persistence in [§ Frame](#frame).



Behavior (besides color)
------

Vinyl records are extensible records, convenient for expressing a program's dataflow
requirements, in that the data can be collected into groups and named like any record,
and things can be written to only depend on the part of the record they use.

Some language extensions will be needed for working with them.

< {-# LANGUAGE DataKinds, FlexibleContexts, TypeOperators #-}

< import Data.Vinyl hiding ((<+>))

This wire takes a Vinyl record whose fields have those of the passed in rendering
function as a subset and renders according to that function.

> renderWire :: (Monoid e, renderdata <: i) =>
> 	(FieldRec renderdata -> IO ())
> 	-> Wire s e GameMonad (FieldRec i) ()
> renderWire rfn = mkGen_ $ \appInfo -> lift $ rfn (rcast appInfo) >> (return $ Right ())

This wire produces the position of the circle. It simply follows the mouse cursor
but negates the y-value. The origin of the mouse coordinates are in the top left
corner of the screen with the y-axis pointing down while the y-axis for rendering
points up.

> posWire :: Monoid e => Wire s e GameMonad a (V2 GL.GLfloat)
> posWire = mouseCursor >>> (second $ arr negate) >>> (arr $ uncurry V2)

Wire that behaves like the identity wire until Q is pressed, then inhibits forever.
We can compose our main gameWire with this wire to quit the program when q is pressed.

> quitWire :: Monoid e => Wire s e GameMonad a a
> quitWire = (mkId &&& eventWire) >>> (rSwitch mkId)
>   where
>     eventWire :: Monoid e => Wire s e GameMonad a (Event (Wire s e m a a))
>     eventWire = (keyPressed GLFW.Key'Q >>> pure mkEmpty >>> now) <|> never

Recall:

< type CursorCircleStyle = '[ '("offset", V2 GL.GLfloat), '("color", V3 GL.GLfloat) ]

We construct records with that as its fields by using (Vy.<+>) to combine
constructions of singleton records, themselves constructed using (SField =:).
SField can be given different types for different fields, letting you name the field
used and then write things like (position =: 5).

< import qualified Data.Vinyl as Vy ((<+>))

This wire expresses our overall behavioral structure. It feeds the position and color
into the rendering loop but inhibits if q is pressed, which makes the program quit,
as we see later in (runGame).

> gameWire :: (HasTime t s, Monoid e) =>
> 	(FieldRec CursorCircleStyle -> IO ())
> 	-> Wire s e GameMonad a ()
> gameWire rfn = quitWire
> 	>>> posWire &&& colorWire
> 	>>> arr (\(pos, color) -> SField =: pos Vy.<+> SField =: color :: FieldRec CursorCircleStyle)
> 	>>> (renderWire rfn)

It's configured from a shader function whose inputs are records with (CursorCircleStyle)
for fields, meaning it's compatible with the way position and color variables are
passed to the (cursorCircle) shaders.

< import CursorProgram (CursorCircleStyle, cursorCircle)



Frame
------

Once we have an OpenGL context and its input controller is set up with
netwire-input, we set up to enter the main loop.

> run :: GLFW.Window -> GLFWInputControl -> IO ()
> run win ictl = do
>   -- initialize the input
>   ipt <- getInput ictl

Binding this loads the shaders & compiles the shader program.
It can be done per-frame (slowly) or on scene change just as well.
We'll be binding shaders through FRP arrows in the later examples.

>   cursCirc <- cursorCircle

All the wire flow is encased in a looping session & wire stepper.

For help typing this loop, our implicit session type could be

< type GameSession = Session IO (Timed Float ())

A session & wire step is like a frame of wire computation, and while stepping the session
we update the time passed to wires to reflect the time since the last update, poll for
input, & check for termination states (reflected in the Netwire inhibition type used),
including errors. It also performs the usual per-frame OpenGL operations.

>   runGame ipt (countSession_ 0.02) (gameWire cursCirc)

>   where

>     -- This loop takes the current input state, the time session and
>     -- our main game wire, and steps the wire until it inhibits.
>     runGame ipt sess w = do

>       -- Before rendering clear the framebuffer
>       GL.clearColor GL.$= GL.Color4 0.0 0.0 0.0 1
>       GL.clear [GL.ColorBuffer]

>       -- Poll the current input
>       ipt' <- pollGLFW ipt ictl

>       -- Figure out our next timestep
>       (timeState, sess') <- stepSession sess

This example can be seen as demonstrating that, while (stepWire) runs the (Wire)
first passed to (runGame) over the current session data, that (Wire) can alter itself
in ways that this stepping loop preserves in the output of (stepWire).

Since the GameMonad is a 'StateT GLFWInputState m', in order to
step the wires, we have to extract the value from our wire. That means
that when we runStateT, we will get the results of our wire and a new
state (for example if the wire debounced any keys). This is what we pass
back to GLFW.

>       --  renderPrg :: IO ((Either e (), Wire s e GameMonad a ()), GLFWInputState)
>       let renderPrg = runGLFWInputT (stepWire w timeState (Right undefined)) ipt'

>       -- Now run the actual IO program to extract the values from it.
>       ((result, w'), ipt'') <- renderPrg

>       -- End of frame cleanup
>       GL.flush
>       GLFW.swapBuffers win

The inhibition monoid (e) is (()).
The termination symbols are (Left ()) and error strings.
We obtain (Right ()) from every successful run of the (Wire), but could produce
useful data with the (Wire) every frame instead.

>       -- Our quit condition is if the OS asked us to quit, or the wire inhibits
>       -- (i.e. someone hit the Q key)
>       case result of
>         Left () -> return ()
>         Right () -> do
>           q <- GLFW.windowShouldClose win
>           unless q $ runGame ipt'' sess' w'



Configuration
------

< import qualified Control.Monad as Monad (when)
< import Data.Maybe (isNothing)
< import System.Directory (getCurrentDirectory, setCurrentDirectory)

> initGL :: String -> Int -> Int -> IO (GLFW.Window, GLFWInputControl)
> initGL windowTitle width height = do
> 	currDir <- getCurrentDirectory

> 	r <- GLFW.init
> 	Monad.when (not r) (error "Error initializing GLFW!")

> 	-- GLSL version determined by GL version
> 	GLFW.windowHint $ GLFW.WindowHint'ClientAPI GLFW.ClientAPI'OpenGL
> 	GLFW.windowHint $ GLFW.WindowHint'OpenGLForwardCompat True
> 	GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
> 	GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
> 	GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 2

> 	m@(~(Just w)) <- GLFW.createWindow width height windowTitle Nothing Nothing
> 	Monad.when (isNothing m) (error "Couldn't create window!")

> 	GLFW.makeContextCurrent m

> 	-- Hack for retina displays
> 	(szx, szy) <- GLFW.getFramebufferSize w
> 	GL.viewport GL.$= (GL.Position 0 0, GL.Size (fromIntegral szx) (fromIntegral szy))

> 	setCurrentDirectory currDir

> 	mkInputControl w >>= (\x -> return (w, x))

> main :: IO ()
> main = initGL "Netwire Input Demo" 500 500 >>= uncurry run
