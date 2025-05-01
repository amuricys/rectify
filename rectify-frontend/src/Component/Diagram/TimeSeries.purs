module Component.Diagram.TimeSeries where

-- import Prelude

-- import Backend (DiagramData(..))
-- import Component.Diagram as Diagram
-- import Control.Monad.State (modify)
-- import Data.Array.NonEmpty (NonEmptyArray, length, mapWithIndex, singleton, tail, toArray)
-- import Data.Maybe (Maybe(..))
-- import Diagram.Surface as Diagram.Surface
-- import Diagram.TimeSeries (mkDataPointNode)
-- import Diagram.TimeSeries as TimeSeries
-- import Effect.Class (class MonadEffect)
-- import Halogen as H
-- import Type.Data.Peano (class IsNat)
-- import Type.Data.Peano.Nat (Nat, D50, reflectNat)
-- import Type.Prelude (Proxy(..))
-- import Unsafe.Coerce (unsafeCoerce)


-- data Query a = ReceiveEnergy { fitness :: Number, betaCounter :: Int } a

-- -- We keep curSize for efficiency, so we don't have to traverse the list every time
-- newtype Queue (n :: Nat) (a :: Type) = Queue { vals :: NonEmptyArray a }

-- queue :: forall @n a. a -> Queue n a
-- queue a = Queue { vals: singleton a }

-- enqueue :: forall n a. IsNat n => a -> Queue n a -> Queue n a
-- enqueue val (Queue { vals }) =
--   let
--     newVals = vals <> singleton val
--   in
--     Queue
--       { vals:
--           if length newVals > (reflectNat (Proxy @n)) then
--             unsafeCoerce (tail newVals) -- Safe because we just checked that newVals longer than maxSize
--           else newVals
--       }

-- queueToDiagramData :: forall n a nodeData linkData. IsNat n => (Int -> a -> Record nodeData) -> Queue n a -> DiagramData nodeData linkData
-- queueToDiagramData f (Queue { vals }) =
--   let
--     nodes = toArray $ mapWithIndex f vals
--   in
--     DiagramData { nodes, links: [] }

-- handleQuery
--   :: forall n m a nodeData linkData
--    . IsNat n
--   => MonadEffect m
--   => ({ fitness :: Number, betaCounter :: Int } -> a)
--   -> Query a
--   -> H.HalogenM (Diagram.State (Queue n a)) Diagram.Action () Void m (Maybe a)
-- handleQuery f = case _ of
--   -- Here we'll talk to GoJS
--   ReceiveEnergy msg a -> do
--     modify (\x -> x { state = enqueue (f msg) x.state }) >>= \x -> case x.diagram of
--       Just d -> do
--         Diagram.updateDiagram d (queueToDiagramData  x.state) a
--       Nothing -> pure $ Just a

-- component :: forall i m. String -> Number -> MonadEffect m => H.Component Query i Void m
-- component divId firstDatapoint 
--   = Diagram.diagramComponent (queue @D50 { fitness: firstDatapoint, betaCounter: 0 }) Diagram.Surface.diag handleQuery