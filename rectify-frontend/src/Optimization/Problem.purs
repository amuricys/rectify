module Optimization.Problem where

import Prelude

import Data.Array (length, zip, (..))
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Codec.Argonaut.Sum as CAS
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), uncurry)
import Diagram.Surface as Diagram.Surface
import Diagram.TSP as Diagram.TSP


type Point2D = { x :: Number, y :: Number }

point2DCodec :: CA.JsonCodec Point2D
point2DCodec = CA.object "Point2D" 
  (CAR.record 
    { "x": CA.number
    , "y": CA.number
    })

type SurfaceSolution = { inner :: Array Point2D, outer :: Array Point2D }
surfaceSolutionCodec :: CA.JsonCodec SurfaceSolution
surfaceSolutionCodec = CA.object "SurfaceSolution" 
  (CAR.record 
  { "inner": CA.array point2DCodec
  , "outer": CA.array point2DCodec
  })

type TSPSolution = { cities :: Array Point2D }
tspSolutionCodec :: CA.JsonCodec TSPSolution
tspSolutionCodec = CA.object "TSPSolution" 
  (CAR.record 
    { "cities": CA.array point2DCodec
    })

type ReservoirSolution = { network :: Array Point2D }
reservoirSolutionCodec :: CA.JsonCodec ReservoirSolution
reservoirSolutionCodec = CA.object "ReservoirSolution" 
  (CAR.record 
    { "network": CA.array point2DCodec
    })

data ProblemTags = SurfaceTag | TSPTag | ReservoirTag | Surface3DTag | TSP3DTag

data Problem
  = Surface SurfaceSolution
  | TSP TSPSolution
  | Reservoir ReservoirSolution
  | Surface3D SurfaceSolution
  | TSP3D TSPSolution

data GoJSProblem 
  = GoJSSurface SurfaceSolution
  | GoJSTSP TSPSolution
  | GoJSReservoir ReservoirSolution

data ThreeJSProblem 
  = ThreeJSSurface3D SurfaceSolution
  | ThreeJSTSP3D TSPSolution

data RendererProblem
  = GoJS GoJSProblem
  | ThreeJS ThreeJSProblem

rendererProblem :: Problem -> RendererProblem
rendererProblem = case _ of
  Surface solution -> GoJS (GoJSSurface solution)
  TSP solution -> GoJS (GoJSTSP solution)
  Reservoir solution -> GoJS (GoJSReservoir solution)
  Surface3D solution -> ThreeJS (ThreeJSSurface3D solution)
  TSP3D solution -> ThreeJS (ThreeJSTSP3D solution)

problemCodec :: CA.JsonCodec Problem
problemCodec = CAS.taggedSum "Problem" 
  (case _ of
    SurfaceTag -> "surface"
    TSPTag -> "tsp"
    ReservoirTag -> "reservoir"
    Surface3DTag -> "surface3d"
    TSP3DTag -> "tsp3d"
  )
  (case _ of
    "surface" -> Just SurfaceTag
    "tsp" -> Just TSPTag
    "reservoir" -> Just ReservoirTag
    "surface3d" -> Just Surface3DTag
    "tsp3d" -> Just TSP3DTag
    _ -> Nothing
    )
  (case _ of
    SurfaceTag -> Right (map Surface <<< CA.decode surfaceSolutionCodec)
    TSPTag -> Right (map TSP <<< CA.decode tspSolutionCodec)
    ReservoirTag -> Right (map Reservoir <<< CA.decode reservoirSolutionCodec)
    Surface3DTag -> Right (map Surface3D <<< CA.decode surfaceSolutionCodec)
    TSP3DTag -> Right (map TSP3D <<< CA.decode tspSolutionCodec)
  )
  (case _ of
    Surface solution -> Tuple SurfaceTag (Just (CA.encode surfaceSolutionCodec solution))
    TSP solution -> Tuple TSPTag (Just (CA.encode tspSolutionCodec solution))
    Reservoir solution -> Tuple ReservoirTag (Just (CA.encode reservoirSolutionCodec solution))
    Surface3D solution -> Tuple Surface3DTag (Just (CA.encode surfaceSolutionCodec solution))
    TSP3D solution -> Tuple TSP3DTag (Just (CA.encode tspSolutionCodec solution))
  )

type DiagramData nodeData linkData = { nodes :: Array (Record nodeData), links :: Array (Record linkData) }

cyclicalArrayToDiagramData :: String -> Int -> Array Point2D -> DiagramData Diagram.Surface.NodeData Diagram.Surface.LinkData
cyclicalArrayToDiagramData cat start points = {
  nodes: map (uncurry toPoint) (zip (start .. (start + length points)) points)
  , links: map (toLink start (start + length points - 1)) (start .. (start + length points - 1))
  }
  where 
    toPoint :: Int -> Point2D -> Record Diagram.Surface.NodeData
    toPoint id p = { key: id, loc: show p.x <> " " <> show p.y, category: cat }
    toLink :: Int -> Int -> Int -> Record Diagram.Surface.LinkData
    toLink first last i = { key: i, from: i, to: if i /= last then i + 1 else first, category: cat }

solutionToSurfaceDiagramData :: SurfaceSolution -> DiagramData Diagram.Surface.NodeData Diagram.Surface.LinkData
solutionToSurfaceDiagramData { inner, outer } = 
  cyclicalArrayToDiagramData "Outer" 0 outer <> cyclicalArrayToDiagramData "Inner" (length outer) inner

solutionToTSPDiagramData :: TSPSolution -> DiagramData Diagram.TSP.NodeData Diagram.TSP.LinkData
solutionToTSPDiagramData { cities } = 
  cyclicalArrayToDiagramData "Cities" 0 cities
