module Component.Diagram.Energy where

import Prelude

import Backend (DiagramData(..))
import Component.Diagram as Diagram
import Control.Monad.State (modify)
import Data.Array.NonEmpty (NonEmptyArray, length, mapWithIndex, singleton, tail, toArray, (..))
import Data.Maybe (Maybe(..))
import Diagram.Surface as Diagram.Surface
import Diagram.TimeSeries (mkDataPointNode, mkLink)
import Diagram.TimeSeries as TimeSeries
import Effect.Class (class MonadEffect)
import Halogen as H
import Type.Data.Peano (class IsNat)
import Type.Data.Peano.Nat (Nat, D50, reflectNat)
import Type.Prelude (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

canvasDivId :: String
canvasDivId = "energyDiv"

data Query a = ReceiveEnergy { fitness :: Number, betaCounter :: Int } a

-- We keep curSize for efficiency, so we don't have to traverse the list every time
newtype Queue (n :: Nat) (a :: Type) = Queue { vals :: NonEmptyArray a }

queue :: forall @n a. a -> Queue n a
queue a = Queue { vals: singleton a }

enqueue :: forall n a. IsNat n => a -> Queue n a -> Queue n a
enqueue val (Queue { vals }) =
  let
    newVals = vals <> singleton val
  in
    Queue
      { vals:
          if length newVals > (reflectNat (Proxy @n)) then
            unsafeCoerce (tail newVals) -- Safe because we just checked that newVals longer than maxSize
          else newVals
      }

queueToDiagramData :: forall n. IsNat n => Queue n { fitness :: Number, betaCounter :: Int } -> DiagramData TimeSeries.NodeData TimeSeries.LinkData
queueToDiagramData (Queue { vals }) =
  let
    nodes = toArray $ mapWithIndex (\i x -> mkDataPointNode (-i) x.fitness x.betaCounter x.fitness) vals
    links = toArray $ map (\x -> mkLink x (x + 1)) (0 .. (length vals - 2))
  in
    DiagramData { nodes, links }

handleQuery
  :: forall n m a
   . IsNat n
  => MonadEffect m
  => Query a
  -> H.HalogenM (Diagram.State (Queue n { fitness :: Number, betaCounter :: Int })) Diagram.Action () Void m (Maybe a)
handleQuery = case _ of
  -- Here we'll talk to GoJS
  ReceiveEnergy msg a -> do
    modify (\x -> x { state = enqueue { fitness: msg.fitness, betaCounter: msg.betaCounter } x.state }) >>= \x -> case x.diagram of
      Just d -> do
        Diagram.updateDiagram d (queueToDiagramData x.state) a
      Nothing -> pure $ Just a

component :: forall i m. Number -> MonadEffect m => H.Component Query i Void m
component firstDatapoint 
  = Diagram.diagramComponent (queue @D50 { fitness: firstDatapoint, betaCounter: 0 }) canvasDivId Diagram.Surface.diag handleQuery