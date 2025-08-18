module Optimization.Renderer.Problem where

import Prelude

import Data.Variant as V

type Point2D = { x :: Number, y :: Number }
type SurfaceSolution = { inner :: Array Point2D, outer :: Array Point2D }
type TSPSolution     = { cities :: Array Point2D }
type ReservoirSolution = { network :: Array Point2D }

-- what GoJS can render
type GoJSRow =
  ( surface   :: SurfaceSolution
  , tsp       :: TSPSolution
  , reservoir :: ReservoirSolution
  )

-- what ThreeJS can render
type ThreeRow =
  ( surface3D :: SurfaceSolution
  , tsp3D     :: TSPSolution
  )

type GoJSProblem   = V.Variant GoJSRow
type ThreeProblem  = V.Variant ThreeRow

data RenderProblem
  = GoJS GoJSProblem
  | Three ThreeProblem