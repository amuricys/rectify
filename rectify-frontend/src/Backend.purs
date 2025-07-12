module Backend where

import Prelude

import Backend.Optimization as Optimization
import Backend.Dynamics as Dynamics

data Payload
  = Optimization Optimization.Payload 
  | Dynamics Dynamics.Payload





-- parseBackend :: String -> Either String Payload

-- parseBackend = 
-- ──────────────────────────────────────────────────────
-- example payloads

x = {"visualize": "optimization"
    ,"datapoint": {
      "problem": "surface",
      "solution": {
          "inner": [
            { "x": 0, "y": 0 },
            { "x": 1, "y": 1 },
            { "x": 2, "y": 2 }
          ],
          "outer": [
            { "x": 0, "y": 0 },
            { "x": 1, "y": 1 },
            { "x": 2, "y": 2 }
          ]
      },
      "optimization-data": {
        "algorithm": "simulated-annealing",
        "algorithm-data": {
          "beta": 0.1,
          "betaCounter": 0,
          "fitness": 0.1
        }
      }
    }
}

y = { "visualize": "dynamics"
    , "datapoint": {
      "system": "lorenz",
      "state": {
        "x": 0,
        "y": 0,
        "z": 0
      },
      "params": {
        "sigma": 10,
        "rho": 28,
        "beta": 8/3
      },
      "t": 0
    }
}