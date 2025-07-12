module Backend.Optimization where

import Backend.Optimization.Algorithm as Algorithm
import Backend.Optimization.Problem as Problem

newtype Payload = Payload
  { solution :: Problem.Problem
  , algorithm :: Algorithm.AlgorithmData 
  }



