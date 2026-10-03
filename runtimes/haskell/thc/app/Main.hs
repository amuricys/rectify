module Main (main) where

import Control.Monad (unless)
import Rectify.Geometry.Segments

main :: IO ()
main = do
  let square = [(0, 0), (1, 0), (1, 1), (0, 1)]
      diagonal = ((0, 0), (1, 1))
      opposite = ((0, 1), (1, 0))
      touching = ((1, 1), (2, 0))
      overlap = ((0.25, 0.25), (0.75, 0.75))
      parallel = ((0, 2), (1, 3))
      checks =
        [ ("counterclockwise area", signedTwiceArea square == 2)
        , ("clockwise area", signedTwiceArea (reverse square) == -2)
        , ("empty area", signedTwiceArea [] == 0)
        , ("proper crossing", properIntersection diagonal opposite)
        , ("crossing symmetry", properIntersection opposite diagonal)
        , ("endpoint contact excluded", not (properIntersection diagonal touching))
        , ("collinear overlap excluded", not (properIntersection diagonal overlap))
        , ("parallel segments", not (properIntersection diagonal parallel))
        , ("zero length segment", not (properIntersection ((0, 0), (0, 0)) diagonal))
        , ("unordered pair count", countProperIntersections [diagonal, opposite, parallel] == 1)
        ]
  mapM_ (\(name, passed) -> unless passed (fail name)) checks
  putStrLn "geometry-probe: 10 checks passed"
