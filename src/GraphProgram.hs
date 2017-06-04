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

col :: SField Color
col = SField

type ProjInfo2D = '[ '("offset", V2 GLfloat) ]

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

-----
-- Graph drawing
-- + Derived circle drawing
-----

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
