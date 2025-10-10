module Test.Codec
  ( problemCodecTest
  )
  where

import Prelude

import Data.Argonaut.Parser as J
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console (log)
import Optimization.Problem as Problem
import Test.QuickCheck (assertEquals)


problemCodecTest :: Effect Unit
problemCodecTest = do
  let expected = Problem.Surface { inner: [{x: 0.0, y: 0.0}, {x: 1.0, y: 0.0}, {x: 1.0, y: 1.0}, {x: 0.0, y: 1.0}], outer: [{x: 0.5, y: 0.5}, {x: 0.5, y: 0.6}, {x: 0.6, y: 0.6}, {x: 0.6, y: 0.5}] }
  let actual = do
        json <- J.jsonParser "{\"solution\":{\"surface\":{\"inner\":[[0,0],[1,0],[1,1],[0,1]], \"outer\":[[0.5,0.5],[0.5,0.6],[0.6,0.6],[0.6,0.5]]}},\"algorithm\":{\"simulatedAnnealing\":{\"beta\":0.9999999999999999,\"betaCounter\":0}}}"
        lmap show $ CA.decode Problem.problemCodec json
  let x = assertEquals (Right expected) actual
  log (show x)
  pure unit
  




