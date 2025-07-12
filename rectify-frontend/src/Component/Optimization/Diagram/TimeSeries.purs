module Component.Optimization.Diagram.TimeSeries where

import Prelude

import Component.Optimization.Diagram (DiagramData(..))
import Component.Optimization.Diagram as Diagram
import Control.Monad.State (modify)
import Data.Array (length, mapWithIndex, singleton, tail, (..))
import Data.Maybe (Maybe(..))
import Data.Reflectable (class Reflectable, reflectType)
import Diagram.TimeSeries as Diagram.TimeSeries
import Effect.Class (class MonadEffect)
import Halogen as H
import Type.Prelude (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

data Query b a = Receive b a


newtype Queue (n :: Int) (a :: Type) = Queue { vals :: Array a }

queue :: forall @n a. Queue n a
queue = Queue { vals: [] }

enqueue :: forall (n :: Int) a. Reflectable n Int => a -> Queue n a -> Queue n a
enqueue val (Queue { vals }) =
  let
    newVals = vals <> singleton val
  in
    Queue
      { vals:
          if length newVals > (reflectType (Proxy @n)) then
            case tail newVals of
              Just t -> t
              Nothing -> unsafeCoerce newVals -- Safe because we just checked that newVals longer than maxSize
          else newVals
      }

queueToDiagramData :: forall n a nodeData linkData. Reflectable n Int => (Int -> a -> Record nodeData) -> (Int -> Record linkData) -> Queue n a -> DiagramData nodeData linkData
queueToDiagramData f g (Queue { vals }) =
  let
    nodes = mapWithIndex f vals
    links = map g (0 .. (length vals - 2))
  in
    DiagramData { nodes, links }

handleQuery
  :: forall n m b dp a nodeData linkData
   . Reflectable n Int
  => MonadEffect m
  => (b -> dp)
  -> (Int -> dp -> Record nodeData)
  -> (Int -> Record linkData)
  -> Query b a
  -> H.HalogenM (Diagram.State (Queue n dp)) Diagram.Action () Void m (Maybe a)
handleQuery receiveToDatapoint toNode toLink = case _ of
  -- Here we'll talk to GoJS
  Receive msg a -> do
    modify (\x -> x { state = enqueue (receiveToDatapoint msg) x.state }) >>= \x -> case x.diagram of
      Just d -> do
        Diagram.updateDiagram d (queueToDiagramData toNode toLink x.state) *> pure (Just a)
      Nothing -> pure $ Just a

component
  :: forall @queueSize i m b dp nodeData linkData
   . MonadEffect m
  => Reflectable queueSize Int
  => String
  -> (b -> dp)
  -> (Int -> dp -> Record nodeData)
  -> (Int -> Record linkData)
  -> H.Component (Query b) i Void m
component divId toDatapoint toNode toLink = Diagram.diagramComponent (queue @queueSize) divId Diagram.TimeSeries.diag
  ( handleQuery
      toDatapoint
      toNode
      toLink
  )