module GoJS.L where

import Data.Lens.Traversal
import Prelude

import Data.Lens (Lens, lens)
import GoJS.GraphObject.Panel.Part.Link.Properties (_midAngle)
import GoJS.GraphObject.Types (Link_)
import GoJS.Unsafe.Set (setUnsafe)

-- so what i want is to run an effectful computation in the path
-- specified by a certain lens on a deeply nested structure

hmm :: forall s t a b m. Monad m => Lens s t a b -> (a -> m b) -> s -> m t
hmm l f s = traverseOf l f s

-- _midAngle' :: Lens Link_ Link_ Number Number
-- _midAngle' = lens _midAngle (\l x -> setUnsafe { _midAngle = x })

{-
desired usage:
do 
  angle <- someLink # _incomingLink <=< _source <=< _incomingLink <=< _midAngle
  someLink #~ (_incomingLink <=< _source <=< _incomingLink <=< _midAngle) 1.0
  angle2 <- someLink # _incomingLink <=< _source <=< _incomingLink <=< _midAngle
  pure $ "old vs new angle: " <> show angle <> " vs " <> show angle2
-}


