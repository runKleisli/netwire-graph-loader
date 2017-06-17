# netwire-graph-loader

Loading a graph (vertices & undirected edges) from files then displaying it on screen.
Features Netwire/GLFW/VinylGL/netwire-input/STM loading screen.
Semi-tutorialized by implementing required behavior across different executables.

No effort is made to catch errors from file load, `forkIO`, or STM. See the documentation from [System.IO](https://hackage.haskell.org/package/base-4.7.0.1/docs/System-IO.html) & [Control.Concurrent](https://hackage.haskell.org/package/base-4.7.0.1/docs/Control-Concurrent.html) (forkIO), & from [the stm package](https://hackage.haskell.org/package/stm-2.4.2) though the primary reference is [Real World Haskell - Chapter 28. Software transactional memory](http://book.realworldhaskell.org/read/software-transactional-memory.html).

The important thing to remember for understanding the control flow of the STM examples is that Haskell is call-by-need (lazy), needing an IO value written in (do) notation is the same as needing the last line's value, and `seq`, `deepseq`, and bang patterns are means of forcing something's evaluation to be a part of evaluating what's needed.

## Usage

__Building__

Generally, a sandboxed build is recommended

```
cabal sandbox init
```

a multithreaded installation is recommended

```
cabal install -j4
cabal build
```

After modifying the source but not dependencies, only `cabal build` is necessary.

__Installing__

Once built, copy the binaries from the `dist/build/...` subdirectory into a fixed directory. Copy also the `etc` folder into that directory. One can copy the `graphs` folder in as well, or follow the sections on GraphLoaderLoadReporting & GraphLoadingScr for making the files they need & how to use a custom graph with them.

__Running__

Run the executables from the shell. Otherwise, the asset files may not be loaded, & you'll get errors of the form

```
GraphLoadingScr: user error (Pattern match failure in do expression at src/GraphProgram.hs:line#:#-#)
```

or

```
GraphLoadingScr: etc/cursor.vert: openBinaryFile: does not exist (No such file or directory)
```

## GraphLoaderCtrl
## CtrlExample.(l)hs ##

Demonstrates in Netwire the kind of permanent state change needed in a loading screen. The cursor has a time-varying color when the program starts, but when the user hits the `C` key, it becomes green. Color responds to pressing the `R`, `G`, & `B` keys before & after, as in the example this is based off of.

Builds on the Style 3 [netwire-vinylglfw-examples](https://github.com/runKleisli/netwire-vinylglfw-examples) port of the Cursor example from [netwire-input-glfw](https://github.com/Mokosha/netwire-input-glfw).

The key change is to `colorWire`, by taking its `(<|>)`-separated list of ways the wire can respond to keypresses to change the color it emits, and replacing the default option of a pulsing color with a `Wire` that behaves like a pulsing color until `C` is pressed, from w/c point on it emits the color green. This `Wire` is based off of `quitWire`, w/c comes from the Cursor example, and the pulsing color `Wire` it takes the place of. `quitWire` is a "wire that behaves like the identity wire until Q is pressed, then inhibits forever."

## GraphLoaderProgressReporting
## ReportingProgressExample.(l)hs ##

STM example wherein a computation forked off into a different thread reports its completion status to the root computation.

Although the concurrent web link checker example from [RWH - STM](http://book.realworldhaskell.org/read/software-transactional-memory.html) uses many of the features we need, it's significantly less straightforward, so to understand our code it would be clearer to read the documentation for Control.Concurrent.forkIO & everything that comes from the stm package.

## GraphLoaderLoadReporting
## ReportingLoadExample.hs ##

STM example wherein a file is loaded and its progress reported as it loads chunks. The load is done in its own thread, so the root computation can interact w/ the user while the file is loading.

__Usage__

A file called `GraphEdgeInds.txt` must be placed in a `graphs` subdirectory of the location of the executable. It must contain a string representation of a `[[Int]]` as would be accepted by Haskell's `read`, w/ no other content, no whitespace including trailing newlines, ex. `[]` or `[[10,1],[2,3]]` or `[[],[2,3]]`. It should be multiple kilobytes big to see the effects well.

## GraphLoadingScr
## GraphLoadingScr.hs ##

This executable loads the graph, then displays it. It uses the completed loading screen.

Before the file is loaded, the program displays a while circle used as a cursor. Once the user hits `L`, the graph begins loading. While it's loading, the cursor circle oscillates in hue. Each time a chunk of a file loads, the cursor gets darker. Once both files of the graph are loaded, the cursor becomes that graph, displayed in green. For simplicity, only the edges are drawn.

__Usage__

The graph to be loaded must be copied into a subfolder called `graphs` of the install directory, and it must be formatted as follows:

* Files called `GraphVerts.txt` and `GraphEdgeInds.txt` contain the data specifying the graph.
* `GraphVerts.txt` specifies the location of the vertices in 2D space as a string representation of a `[[Double]]`.
	* In particular, a `Double` is written out as a string, like `5`, `5.0`, or `-1.0009012168253895e-2`.
	* The 2 coordinates of a point are written as a `[Double]` in string representation with exactly 2 entries, such as `[5.0, 1.3]`.
	* The file itself is the list `[_]` of all the graph's vertices coordinates in string representation. Each vertex of the graph is listed in order like `[a, b, ...]`, w/ `a`, `b` their coordinate lists, written as above.
	* The order the vertex appears in the list, starting from `0`, is the index of the vertex.
	* The file doesn't contain any whitespace or other data.
* `GraphEdgeInds.txt` specified the edges as a string representation of a `[[Int]]`.
	* The 2 vertices of an edge are written as a `[Int]` in string representation with exactly 2 entries, such as `[20, 0]`. This connects vertex `20` and vertex `0`, w/ the numbers referring to the order they appear in the vertex file.
	* The file itself is the list `[_]` of all the graph's edges in string representation, where the string representation of an edge is as specified above.
	* The file doesn't contain any whitespace or other data.

__State & resource aquisition__

This uses local state control, in that switching `Wire`s is used to go from initial to loading to a state w/c emits the file contents then becomes the contents as loaded. Alternatives might be aquiring resources on the input signal or arguments to `gameWire`, or adding a `StateT` or `ReaderT` to the monad transformer stack `GameMonad` to allow program-specific state to be written and read over multiple passes through `gameWire` or `run.runGame`. The latter approach to state control is taken in the post ["Simple netwire program"](https://codereview.stackexchange.com/questions/27701/simple-netwire-program).

The state carried in those approaches has a global lifetime, indeed a life outside `Wire`s. All 3 approaches can be mixed, and the last of them is already used as `FRP.Netwire.Input.GLFW.GLFWInputT`, but the latter 2 require fundamental changes to the control flow and typing of the program to implement or alter, depending on what's needed. Hence, they are best avoided for storing resources that can change structure over the course of development, although if state is expressed as an [extensible record](https://wiki.haskell.org/Extensible_record), this can allow some extra development flexibility there, like swapping the state record used with a different extension of the minimal record needed to express what's done between applications of `stepWire`.

It's also possible to store information in the inhibition values of a wire, but this stores termination information, and checking termination is a control point where code is better more static over development and where it's not natural to script interactions with general state.

Observation about the kind of state control used: `gameWire` doesn't maintain its consituents' resource aquisition state - the instructions for drawing information into the environment are part of the wire, so when the wire is switched, the mechanism switches with it.

Defining resource aquisition within an arrow that emits or serves the resource fits the needs of programs that acquire or use different kinds of resources at different times or acquire resources on triggering events.

__UX__

Cutting to a circle of different appearance on hitting the load key gives feedback that the user has done something meaningful. If it takes a long time to load the first few chunks, and the cursor changes directly to the same color as the background instead and disappears, the user may not be assured of a progression to an expected state. That the circle is changing color constantly and still moving with the mouse indicates the program is able to update in realtime & interact with the user while the work they're waiting on is being done, reflecting the program's structure in its appearance.

The circle fades into the background as it becomes less relevant, but this prevents one from being able to detect if the load is stuck at a (near-)completed state. This is more clearly represented by a bytes loaded out of total count.

## Future work ##

A literate programming version with more tutorialization, including development of the graph drawing program itself & of the intermediate step from the repo's initialization wherein only one file making up the graph is loaded.

File selection.

If one wants to load an arbitrary number of files, the program has to be significantly generalized.

__Bytepacked files__

We implemented an incremental read of a value's string representation. This makes the files vastly larger than a packed representation, wherein for instance each `Double` would by definition take 64 bits rather than 8+ per character.

This is mainly to make it convenient for prototyping, and allows the code to be reused with diverse outputs from independent Haskell programs, but there are also some accessible 3D model formats using strings for coordinates. Otherwise, many custom file formats will mix strings or other formatted data into packed data.

Beginning with GraphLoaderLoadReporting, this exercize could be redone with binary data reading and parsing. References on this topic include [Haskell Wiki - Dealing with binary data](https://wiki.haskell.org/Dealing_with_binary_data), `incrementalExample` from the [Data.Binary.Get documentation](https://hackage.haskell.org/package/binary/docs/Data-Binary-Get.html), the [Data.Binary.Strict.IncrementalGet documentation](https://hackage.haskell.org/package/binary-strict/docs/Data-Binary-Strict-IncrementalGet.html), ["Optimising Haskell data reading from file"](https://stackoverflow.com/questions/24278006/optimising-haskell-data-reading-from-file), & potentially the [io-streams package](https://hackage.haskell.org/package/io-streams), the [streaming package](https://hackage.haskell.org/package/streaming), or compatible. The world of parsing chunked streams of binary data in Haskell is large.

__Multiple dimensions__

Drawing vertices. This expands the program with a notion of scene. It is then largely trivial to extend this program to 2D simplicial complexes - the meshes to 3D models.

Controls for translation and scaling, and initially depicting the graph expanded so its extents match the screen, will be necessary for most inspections of a graph. For examining ≥3D objects, the appropriate controls for movement and rotation need to be added.

When the object is higher dimension than the one it's to be depicted in, and the projection is to be controlled by the program's user, there are different kinds of extension to consider. GLSL requires the vertex data to be sent to shaders with fixed-size types. Ultimately, a projection to projective 3-space must be chosen.

Since the projection is of data in a fixed type, the shader has to be changed if the incoming vertices' dimension changes. Barring a system for generating the shaders within the program, this means the dimension the shader accepts vertices of must be fixed before the program is run. This also applies if the simplices in each dimension are to have some shader depicting their presence.

If a map to projective 3-space or the space of vertices that can be sent to the shaders is to be applied before the vertices are sent to the shaders:
* One doesn't need to send the projection matrix to the shaders
* The types in the shaders don't need to be changed when the original complex's dimension changes, so the program can be made to handle complexes of undetermined dimension
* One loses freedom in the representation of higher-dimensional data through ambiguities in the projection, and to fix the number of shaders involved one also has to fix the dimension of data meaningful in the depiction.
* The linear algebra is back to being done on the CPU instead of the GPU, and the way the linear algebra is done, hence the types used for the vectors, has a significant effect on speed. `hmatrix` is quite fast.

If the map from the vectors to projective 3-space is done in GLSL:
* The dimension of the complex must be known at compiletime.
* The matrix of the map or other instructions for performing it must be passed to the shader as well.
* One has to write the projection as GLSL. Since GLSL only supports so many dimensions in a vector, some chunking of vectors and matrices and chunkwise manipulation of them has to be done to get the required point in projective 3-space out.
* All the geometric information about the vector is available to the depiction, but it's not nicely formatted. Extracting features from the source data and considering the map to projective 3-space a feature extraction, when sufficient to write the desired shader, is a nice idiom here because that allows the source data in full complexity to only appear early in the shader pipeline or to be passthroughed.

The choice between the two potentially affects whether one needs a matrix representing the effect of the movement and rotation controls sent to the shader. Hence, whether the semantics of the controls should be separated from the controls, and how these should be incorporated into a transformation matrix product, composition of linear endomorphisms, or other associating transformations.

__Questions__

Q: Should we use `keyDebounced` instead of `keyPressed` to admit switching to the loading state? `quitWire` uses `keyPressed`, but that has idempotent behavior under multiple presses.

Q: Is `initialVal` necessary, or do `Wire`s behave like their last values without recomputing when their definition doesn't depend on values that have changed?

Wouldn't it also fulfill the intention for `initialVal` to take a wire, behave as that wire once, then switch to a constant carrying the result of the wire's first run? That's a typically required behavior in arrowized FRP.

Q: Can `initialVal`-guarded computations still run more than once? Or does the fact that the `Event` triggering the `onEventM` only occurs once mean `hold` is constant once the event occurs? What we're looking for is a space leak from `initialVal` wires that doesn't appear when `rSwitch` and the like are used.

## Bibliography

* Mokosha, "[netwire-input-glfw](https://github.com/Mokosha/netwire-input-glfw) - Cursor example"
* runKleisli, "[netwire-vinylglfw-examples](https://github.com/runKleisli/netwire-vinylglfw-examples) - Style 3". Reduction of "netwire-input-glfw - Cursor example" to VinylGL.
* danielpwright, Mokosha. Stack Overflow question - [Correct use of Netwire (5)](https://stackoverflow.com/questions/28595783/correct-use-of-netwire-5#28924645). Re: Inhibition semantics of `(<|>)`d `Wire`s and building state changes into a `Wire` that persist over repeated use of the `Wire` network written.
* O'Sullivan, Stewart, Goerzen. "[Real World Haskell - Chapter 28. Software transactional memory](http://book.realworldhaskell.org/read/software-transactional-memory.html)"
	* At the concurrent web link checker, most relevant information is extracted into a practical form through our Reporting examples, when it comes to loading files and multithreading loading of files, whereas as a learning reference this chapter is suitable for understanding the basic STM forms' behavior & their proper use.
* Lipovača. "[Learn You a Haskell for Great Good! - Input and Output](http://learnyouahaskell.com/input-and-output)".
	* Re: `withFile`, `hSetBuffering`, `hGetContents`.
	* "You can control how exactly buffering is done by using the hSetBuffering function. It takes a handle and a BufferMode and returns an I/O action that sets the buffering. BufferMode is a simple enumeration data type and the possible values it can hold are: NoBuffering, LineBuffering or BlockBuffering (Maybe Int). The Maybe Int is for how big the chunk should be, in bytes. If it's Nothing, then the operating system determines the chunk size."
* [Haskell Wiki contributors](https://wiki.haskell.org/index.php?title=Dealing_with_binary_data&action=history). [Haskell Wiki - Dealing with binary data](https://wiki.haskell.org/Dealing_with_binary_data)
	* "Normal Haskell `String` types are linked lists of 32-bit characters."
	* Working with files consisting of a packed, bytewise representation of data (see "Future work" section)
* [Control.Wire.Switch documentation](https://hackage.haskell.org/package/netwire-5.0.2/docs/Control-Wire-Switch.html). Re: Control flow of state changes.
* [Control.Concurrent documentation](https://hackage.haskell.org/package/base-4.7.0.1/docs/Control-Concurrent.html). For `forkIO`.
* [STM documentation](https://hackage.haskell.org/package/stm-2.4.2)
* [System.IO documentation](https://hackage.haskell.org/package/base-4.7.0.1/docs/System-IO.html)
	* The [buffering operations section](https://hackage.haskell.org/package/base-4.7.0.1/docs/System-IO.html#g:12) in particular.
* Cubic. Code Review Stack Exchange question - "[Simple netwire program](https://codereview.stackexchange.com/questions/27701/simple-netwire-program)". Implementation of permanent color on keypress in Netwire through stored state information.
