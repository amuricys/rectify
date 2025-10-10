module Optimization.Problem where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Array (length, zip, (..))
import Data.Codec.Argonaut (JsonDecodeError)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Codec.Argonaut.Sum as CAS
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Symbol (reflectSymbol)
import Data.Tuple (Tuple(..), uncurry)
import Diagram.Surface as Diagram.Surface
import Diagram.TSP as Diagram.TSP
import Type.Prelude (Proxy(..))


type Point2D = { x :: Number, y :: Number }
type Point3D = { x :: Number, y :: Number, z :: Number }

point2DCodec :: CA.JsonCodec Point2D
point2DCodec = CA.object "Point2D" 
  (CAR.record 
    { "x": CA.number
    , "y": CA.number
    })
point3DCodec :: CA.JsonCodec Point3D
point3DCodec = CA.object "Point3D" 
  (CAR.record 
    { "x": CA.number
    , "y": CA.number
    , "z": CA.number
    })

mapCodec' :: forall a b. (a -> b) -> (b -> a) -> CA.Codec' (Either JsonDecodeError) Json a -> CA.Codec' (Either JsonDecodeError) Json b
mapCodec' f k (CA.Codec g h) = CA.codec (map f <<< g) h'
  where 
    h' :: b -> Json
    h' b = let 
      t = k b
      (Tuple r1 _) = h t
      in 
        r1

newtype SurfaceSolution = SurfaceSolution { inner :: Array Point2D, outer :: Array Point2D }
derive instance genericSurfaceSolution :: Generic SurfaceSolution _
derive newtype instance eqSurfaceSolution :: Eq SurfaceSolution
instance showSurfaceSolution :: Show SurfaceSolution where
  show = genericShow
surfaceSolutionCodec :: CA.JsonCodec SurfaceSolution
surfaceSolutionCodec = mapCodec' SurfaceSolution (\(SurfaceSolution x) -> x) $ CA.object "SurfaceSolution" 
  (CAR.record 
  { "inner": CA.array point2DCodec
  , "outer": CA.array point2DCodec
  })

newtype TSPSolution = TSPSolution { cities :: Array Point2D }
derive instance genericTSPSolution :: Generic TSPSolution _
derive newtype instance eqTSPSolution :: Eq TSPSolution
instance showTSPSolution :: Show TSPSolution where
  show = genericShow
tspSolutionCodec :: CA.JsonCodec TSPSolution
tspSolutionCodec = mapCodec' TSPSolution (\(TSPSolution x) -> x) $ CA.object "TSPSolution" 
  (CAR.record 
    { "cities": CA.array point2DCodec
    })

newtype Surface3DSolution = Surface3DSolution { inner :: Array Point3D, outer :: Array Point3D }
derive instance genericSurface3DSolution :: Generic Surface3DSolution _
derive newtype instance eqSurface3DSolution :: Eq Surface3DSolution
instance showSurface3DSolution :: Show Surface3DSolution where
  show = genericShow
surface3DSolutionCodec :: CA.JsonCodec Surface3DSolution
surface3DSolutionCodec = mapCodec' Surface3DSolution (\(Surface3DSolution x) -> x) $ CA.object "Surface3DSolution" ( 
  (CAR.record 
    { "inner": CA.array point3DCodec
    , "outer": CA.array point3DCodec
    }))

newtype TSP3DSolution = TSP3DSolution { cities :: Array Point3D }
derive instance genericTSP3DSolution :: Generic TSP3DSolution _
derive newtype instance eqTSP3DSolution :: Eq TSP3DSolution
instance showTSP3DSolution :: Show TSP3DSolution where
  show = genericShow
tsp3DSolutionCodec :: CA.JsonCodec TSP3DSolution
tsp3DSolutionCodec = mapCodec' TSP3DSolution (\(TSP3DSolution x) -> x) $ CA.object "TSP3DSolution" 
  (CAR.record 
    { "cities": CA.array point3DCodec
    })

newtype ReservoirSolution = ReservoirSolution { network :: Array Point2D }
derive instance genericReservoirSolution :: Generic ReservoirSolution _
derive newtype instance eqReservoirSolution :: Eq ReservoirSolution
instance showReservoirSolution :: Show ReservoirSolution where
  show = genericShow
reservoirSolutionCodec :: CA.JsonCodec ReservoirSolution
reservoirSolutionCodec = mapCodec' ReservoirSolution (\(ReservoirSolution x) -> x) $ CA.object "ReservoirSolution" 
  (CAR.record 
    { "network": CA.array point2DCodec
    })

data ProblemTags = SurfaceTag | TSPTag | ReservoirTag | Surface3DTag | TSP3DTag

data Problem
  = Surface (Proxy "GoJS") SurfaceSolution
  | TSP (Proxy "GoJS") TSPSolution
  | Reservoir (Proxy "GoJS") ReservoirSolution
  | Surface3D (Proxy "ThreeJS") Surface3DSolution
  | TSP3D (Proxy "ThreeJS") TSP3DSolution

derive instance eqProblem :: Eq Problem
derive instance genericProblem :: Generic Problem _
instance showProblem :: Show Problem where
  show = genericShow

rendererType :: Problem -> String
rendererType prob = case prob of
  Surface p _ -> reflectSymbol p
  TSP p _ -> reflectSymbol p
  Reservoir p _ -> reflectSymbol p
  Surface3D p _ -> reflectSymbol p
  TSP3D p _ -> reflectSymbol p

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
    SurfaceTag -> Right (map (Surface Proxy) <<< CA.decode surfaceSolutionCodec)
    TSPTag -> Right (map (TSP Proxy) <<< CA.decode tspSolutionCodec)
    ReservoirTag -> Right (map (Reservoir Proxy) <<< CA.decode reservoirSolutionCodec)
    Surface3DTag -> Right (map (Surface3D Proxy) <<< CA.decode surface3DSolutionCodec)
    TSP3DTag -> Right (map (TSP3D Proxy) <<< CA.decode tsp3DSolutionCodec)
  )
  (case _ of
    Surface _ solution -> Tuple SurfaceTag (Just (CA.encode surfaceSolutionCodec solution))
    TSP _ solution -> Tuple TSPTag (Just (CA.encode tspSolutionCodec solution))
    Reservoir _ solution -> Tuple ReservoirTag (Just (CA.encode reservoirSolutionCodec solution))
    Surface3D _ solution -> Tuple Surface3DTag (Just (CA.encode surface3DSolutionCodec solution))
    TSP3D _ solution -> Tuple TSP3DTag (Just (CA.encode tsp3DSolutionCodec solution))
  )
class DiagramData p nodeData linkData | p -> nodeData linkData where
  toDiagramData :: p -> { nodes :: Array (Record nodeData), links :: Array (Record linkData) }

instance diagramDataSurface :: DiagramData SurfaceSolution Diagram.Surface.NodeData Diagram.Surface.LinkData where
  toDiagramData (SurfaceSolution { inner, outer }) = 
    cyclicalArrayToDiagramData "Outer" 0 outer <> cyclicalArrayToDiagramData "Inner" (length outer) inner
    where 
      cyclicalArrayToDiagramData cat start points = {
        nodes: map (uncurry toPoint) (zip (start .. (start + length points)) points)
        , links: map (toLink start (start + length points - 1)) (start .. (start + length points - 1))
        }
        where
          toPoint :: Int -> Point2D -> Record Diagram.Surface.NodeData
          toPoint id p = { key: id, loc: show p.x <> " " <> show p.y, category: cat }
          toLink :: Int -> Int -> Int -> Record Diagram.Surface.LinkData
          toLink first last i = { key: i, from: i, to: if i /= last then i + 1 else first, category: cat }

instance diagramDataTSP :: DiagramData TSPSolution Diagram.TSP.NodeData Diagram.TSP.LinkData where
  toDiagramData (TSPSolution { cities }) = 
    cyclicalArrayToDiagramData "Cities" 0 cities
    where 
      cyclicalArrayToDiagramData cat start points = {
        nodes: map (uncurry toPoint) (zip (start .. (start + length points)) points)
        , links: map (toLink start (start + length points - 1)) (start .. (start + length points - 1))
        }
        where
          toPoint :: Int -> Point2D -> Record Diagram.TSP.NodeData
          toPoint id p = { key: id, loc: show p.x <> " " <> show p.y, category: cat }
          toLink :: Int -> Int -> Int -> Record Diagram.TSP.LinkData
          toLink first last i = { key: i, from: i, to: if i /= last then i + 1 else first, category: cat }

