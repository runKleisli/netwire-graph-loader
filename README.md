# netwire-graph-loader

Loading a graph (vertices & undirected edges) from files then displaying it on screen.
Features Netwire/GLFW/VinylGL/netwire-input/STM loading screen.
Semi-tutorialized by implementing required behavior across different executables.

See [README-verbose.md](../blob/master/README-verbose.md) for things like the semantics, concepts, research, & design behind these examples, advanced usage instructions, gaps in development, & future steps.

## Usage

Once built, copy the binaries & the `etc` and `graphs` folders into a fixed directory.

Run the executables from the shell. Otherwise, external files may not be loaded, so they won't run.

## GraphLoaderCtrl (CtrlExample.hs)

Demonstrates in Netwire the kind of permanent state change needed in a loading screen. The cursor has a time-varying color when the program starts, but when the user hits the `C` key, it becomes green. Color responds to pressing the `R`, `G`, & `B` keys before & after, as in the example this is based off of.

## GraphLoaderProgressReporting (ReportingProgressExample.hs)

STM example wherein a computation forked off into a different thread reports its completion status to the root computation.

## GraphLoaderLoadReporting (ReportingLoadExample.hs)

STM example wherein a file is loaded and its progress reported as it loads chunks. The load is done in its own thread, so the root computation can interact w/ the user while the file is loading.

## GraphLoadingScr (GraphLoadingScr.hs)

This executable loads the graph, then displays it. It uses the completed loading screen.

Before the file is loaded, the program displays a while circle used as a cursor. Once the user hits `L`, the graph begins loading. While it's loading, the cursor circle oscillates in hue. Each time a chunk of a file loads, the cursor gets darker. Once both files of the graph are loaded, the cursor becomes that graph, displayed in green. For simplicity, only the edges are drawn.
