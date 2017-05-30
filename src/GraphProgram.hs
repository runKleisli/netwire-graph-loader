{-# LANGUAGE DataKinds, FlexibleContexts, TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
module GraphProgram where

import Data.Vinyl
import Graphics.GLUtil
import Graphics.Rendering.OpenGL hiding (normal, normalize, light, Normal, Color)
import Linear (V2(..), V3(..))
import Graphics.VinylGL
import Foreign.Ptr (nullPtr)
import System.FilePath ((</>))

---

-- import qualified Data.Map as Map
-- import qualified Data.List as L
-- import Linear.V
import Control.Arrow hiding ((<+>))

------------------------------------------------
-- Shader input records
------------------------------------------------

type Pos2D = '("vertexPos", V2 GLfloat)
type Color = '("vertexColor", V3 GLfloat)

pos2D :: SField Pos2D
pos2D = SField

{-
normal :: SField Normal
normal = SField
-}

col :: SField Color
col = SField

type ProjInfo2D = '[ '("offset", V2 GLfloat) ]


------------------------------------------------
-- Graph modeling
------------------------------------------------


{-
This type is not general, b-c for higher-dimensional drawings, our data won't be given
as a fixed-dim output until the projection is chosen, w/c should be user-modifiable &
hence a shader input. However, each special-case type must be fixed-dimension, since
the result data will also be a shader input & the projection will vary in type.

This suggests each shader be judged as compatible w/ a given dimension according to the
type of projection it uses, so that different record constraints are imposed for
projection info requirements corresponding to different dimensions of drawing.

For 2D, the projection info if any is 2x2 mat, and the vertex data type is (V2 GLfloat).
For 3D, the projection info is a 4x4 homogeneous matrix, & vertex type (V3 GLfloat).
For ND, projections are (N+1)d homogeneous matrices split up into blocks, & vertices are
	Nd lists split up into at-most-(V4 GLfloat)-sized chunks.

	To allow the possibility of code generation, the block sizes could be regular,
	if we can arrange that the homogeneous matrix's in-shader application reduces to
	the linear projection to the used dimensions when zero rows & columns are given
	for the coordinates in the unused dimensions of (N+K+1)d space. Urgh.. but no
	matter what, the (N+K+1)d projection matrices will be of different chunks than
	the vertex vectors, unless we homogenize those, w/c we'd end up doing somewhere
	along the pipeline anyway.

On one hand, doing projection on GPU is a fork of the design. On the other hand,
if we switch to doing projection on CPU, we can still use the 2D or 3D-specialized code
& revert to a more traditional, projectionless shader, meaning we only remove code
& insert a projection step in-between gathering vertices & passing them to the shader.

I see.. so, actually, we need to split this into a process for obtaining the ND vector
representation and a later step for accessing data w/ that representation. That way, the
code for accessing the vertices only depends on what the representation type is, so
when the dimension changes between doing projection on GPU and on CPU, access doesn't.

access : [posty] -> Int -> posty
better, access : (vertty -> Int) -> [posty] -> vertty -> posty
Even better, if [posty] is replaced w/ an indexable structure of (posty)s and Int by
its index type.

project : vertty -> posty
or, project : [vertty] -> [posty]

getcoords : [posty] -> Int -> posty
getcoords = flip index

The function argument to the last version of (access) is a getter, w/c is composed w/ the usual getter for [posty] by indices. So really, the last version of (access) can be
replaced w/ any getter of (posty)s from (vertty)s. The projection is such a getter.

So the process for obtaining the [posty] should really be done independently of
specifying the shader programs. It's a different functionality of the application.

The only problem is when there are other parameters to the shader program that come
from interaction w/ the application, for example, changing the color of one vertex.

But it's not as big a deal to decide on figuring out the colors things are supposed to
have near before you send the data off to shaderville. If anything, we'll have more flexibility, since colors are often either based on the preprojection info or done
entirely in the shader. Ugh, but then as we tend toward generality - specifying colors
before gathering points - we end up refocusing the pipeline into main but doing nothing
to localize to a process of deducing shader data from miminal specification.

What I propose is, we build pipelines for incrementally filling out a record of data
required to build a shader or list of shaders. As we change the needs of the program,
we can replace pieces of the pipeline. So, we want to work mostly Record -> Record,
rather than Data -> Record. For this, partial extensions can be encoded as different
subrecord constraints on the input and output types, the output-constraining subrecord
being the transformation of the input-constraining record.

In other words, while adjoining a color induces an upgrade from position-only fields
to color fields, out pipeline for working w/ noncolor data doesn't need to know
whether we have color data yet.
-} 


------------------------------------------------
-- Graph drawing shader programs
------------------------------------------------

-------
-- Drawing info records
-- Records for what the shader programs need to know
-------


type VertDrawInfo = '[ '("vertlistD0", [FieldRec [Pos2D,Color]])
		, '("vindlistD0", [Word32])
		, '("numglpts", GLsizei) ]

type EdgeDrawInfo = '[ '("vertlistD1", [FieldRec [Pos2D,Color]])
		, '("vindlistD1", [Word32])
		, '("numgledges", GLsizei) ]

vertlistD0Ref :: SField '("vertlistD0", [FieldRec [Pos2D,Color]])
vertlistD0Ref = SField

vindlistD0Ref :: SField '("vindlistD0", [Word32])
vindlistD0Ref = SField

numglptsRef :: SField '("numglpts", GLsizei)
numglptsRef = SField

vertlistD1Ref :: SField '("vertlistD1", [FieldRec [Pos2D,Color]])
vertlistD1Ref = SField

vindlistD1Ref :: SField '("vindlistD1", [Word32])
vindlistD1Ref = SField

numgledgesRef :: SField '("numgledges", GLsizei)
numgledgesRef = SField

-- | Num verts -> Num edges -> Graph boilerplate data.
-- An abbreviation.
-- Complements the salient data in a specification of some shader programs' input data.
simplicial1D_countsAndIndexes ::  Int -> Int
	-> FieldRec '[ '("vindlistD0", [Word32])
	, '("numglpts", GLsizei)
	, '("vindlistD1", [Word32])
	, '("numgledges", GLsizei) ]
simplicial1D_countsAndIndexes a b = vindlistD0Ref =: [0..(fromIntegral $ a-1)]
	<+> numglptsRef =: fromIntegral a
	<+> vindlistD1Ref =: [0..(fromIntegral $ (b*2) - 1)]
	<+> numgledgesRef =: fromIntegral b

-------
-- Vertex drawing backend
-- Colored with vertcolors
-------

bakeVert :: V2 GLfloat -> FieldRec '[Pos2D]
bakeVert = (pos2D =:)

bakeEdges :: [[V2 GLfloat]] -> [FieldRec '[Pos2D]]
bakeEdges = concatMap (map (pos2D =:))

{-
All points yellow.
-}
vertcolors :: FieldRec '[Pos2D] -> FieldRec [Pos2D,Color]
vertcolors = (<+> col =: V3 1.0 1.0 0.0)

-------
-- Edge drawing backend
-- Colored per-vert w/ edgecolors
-------

{-
All edges green.
-}
edgecolors :: FieldRec '[Pos2D] -> FieldRec [Pos2D,Color]
edgecolors = (<+> col =: V3 0.0 1.0 0.0)

-------
-- Metafunctions deriving shader programs for every dimension
-------

-----
-- Parameters to circle drawing
-----

cursorNumSlices :: Num a => a
cursorNumSlices = 50

cursorNumIdxs :: GLsizei -- = NumArrayIndices
cursorNumIdxs = toEnum $ cursorNumSlices + 2

cursorpts :: [V2 GLfloat]
cursorpts = take (cursorNumSlices+2) $ (V2 0 0) : (zipWith V2 (map cos winding) (map sin winding))
	where
		winding :: [Float]
		winding = [0,(2*pi/(fromIntegral cursorNumSlices))..]

cursorinds :: [Word32]
cursorinds = take (cursorNumSlices+2) [0,1..]

----
-- Parameters to second circle drawing
----

cursor2pts :: [[Double]]
cursor2pts = take (cursorNumSlices+2) $ [0, 0] : (zipWith (\x y -> x:y:[]) (map cos winding) (map sin winding))
	where
		winding :: [Double]
		winding = [0,(2*pi/(fromIntegral cursorNumSlices))..]

cursor2edges :: [[Int]]
cursor2edges = take cursorNumSlices $ map (\x -> [x, (x+1) `mod` cursorNumSlices+1]) [0..]

-----
-- Circle drawing
-----

cursorCircle :: (ProjInfo2D <: i) => IO (FieldRec i -> IO ())
cursorCircle = graphEdgesProgram2D drawdata
	where
		drawdata = vertlistD0Ref =:
				map (vertcolors . bakeVert)
				cursorpts
			<+> vertlistD1Ref =:
				(take (2*compnumedges)
				>>> map (edgecolors . bakeVert))
				cursorpts
			<+> simplicial1D_countsAndIndexes
				compnumverts
				compnumedges
		compnumverts = length cursorpts
		compnumedges = compnumverts `div` 2

type CompRec1D = '[ '("verts", [[Double]] {- each length = dim -})
	, '("edgeinds", [[Int]] {- each length 2 -}) ]

{-
comp1Dvindref :: SField '("vertinds", [Int])
comp1Dvindref = SField
-}

comp1Dvref :: SField '("verts", [[Double]])
comp1Dvref = SField

comp1Deref :: SField '("edgeinds", [[Int]])
comp1Deref = SField

bestDrawing :: (ProjInfo2D <: i, CompRec1D <: j)
	=> FieldRec j -> IO (FieldRec i -> IO ())
bestDrawing fr = graphEdgesProgram2D drawdata
	where
		{-
		-- IS THIS NOT ENOUGH FOR YOU, CONSTRAINT INFERENCE?!?!?!
		-- data1D = rcast fr :: FieldRec CompRec1D
		drawdata = vertlistD0Ref =:
				(map (vertcolors . bakeVert . mkV2FromDoubles))
				(getField $ rget comp1Dvref
					(rcast fr :: FieldRec CompRec1D))
			<+> vertlistD1Ref =:
				(take (2*compnumedges)
				>>> map (edgecolors . bakeVert))
				cursorpts
			<+> simplicial1D_countsAndIndexes
				compnumverts
				compnumedges
		-}
		yourverts :: [[Double]]
		yourverts = getField $ rget comp1Dvref (rcast fr :: FieldRec CompRec1D)
		youredgeinds :: [[Int]]
		youredgeinds = getField $ rget comp1Deref (rcast fr :: FieldRec CompRec1D)
		youredgeptlist :: [V2 GLfloat]
		youredgeptlist = concatMap (map $ mkV2FromDoubles . (yourverts!!))
			youredgeinds
		drawdata = vertlistD0Ref =:
				(map (vertcolors . bakeVert . mkV2FromDoubles))
				yourverts
			<+> vertlistD1Ref =:
				(map (edgecolors . bakeVert))
				youredgeptlist
			<+> simplicial1D_countsAndIndexes
				compnumverts
				compnumedges
		compnumverts = length yourverts
		compnumedges = length youredgeinds

		mkV2FromDoubles :: [Double] -> V2 GLfloat
		mkV2FromDoubles [] = error "Some Vert didn't have 2 enough coords."
		mkV2FromDoubles (_:[]) = error "Some Vert didn't have 2 enough coords."
		mkV2FromDoubles (x:y:_) = V2 (realToFrac x) (realToFrac y)

cursorCircle2 :: (ProjInfo2D <: i) => IO (FieldRec i -> IO ())
cursorCircle2 = bestDrawing
	$ comp1Dvref =: cursor2pts <+> comp1Deref =: cursor2edges

-----
-- Shader programs
-----

graphEdgesProgram2D :: (EdgeDrawInfo <: j, ProjInfo2D <: i)
	=> FieldRec j -> IO (FieldRec i -> IO ())
graphEdgesProgram2D erec = let !numelems = getField $ rget numgledgesRef (rcast erec :: FieldRec EdgeDrawInfo) in do
		sprog <- simpleShaderProgram ("etc"</>"graph.vert") ("etc"</>"graph.frag")
		vb <- bufferVertices (getField $ rget vertlistD1Ref (rcast erec :: FieldRec EdgeDrawInfo))
		eb <- makeBuffer ElementArrayBuffer (getField $ rget vindlistD1Ref (rcast erec :: FieldRec EdgeDrawInfo))
		vao <- makeVAO $ do
			currentProgram $= Just (program sprog)
			enableVertices' sprog vb
			bindVertices vb
			bindBuffer ElementArrayBuffer $= Just eb
		let ss = setUniforms sprog
		return $ \appInfo -> withVAO vao $ do
			currentProgram $= Just (program sprog)
			ss (rcast appInfo :: FieldRec ProjInfo2D)
			drawElements Lines (numelems*2) UnsignedInt nullPtr
