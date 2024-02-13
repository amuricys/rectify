{-# LANGUAGE TemplateHaskell #-}
module Main where

import Prelude

import Hedgehog
import Hedgehog.Main
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Spec.LinAlg qualified as LinAlg
import Surface


main :: IO ()
main = defaultMain [Hedgehog.checkParallel LinAlg.tests]