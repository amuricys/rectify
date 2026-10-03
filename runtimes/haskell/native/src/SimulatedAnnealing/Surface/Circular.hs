{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}

module SimulatedAnnealing.Surface.Circular where

import Data.Aeson (FromJSON, ToJSON)
import Data.Finite (Finite, finite, getFinite)
import Data.Finite.Integral (weaken)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Type.Equality ((:~:) (Refl))
import Data.Type.Ord (OrderingI (..), type (<), type (>), type (<=))
import Data.Vector.Sized (Vector, generate, index, (++), (//))
import Data.Vector.Sized qualified as V
import Debug.Pretty.Simple (pTraceShow)
import GHC.TypeLits
  ( KnownNat,
    Nat,
    OrderingI (EQI),
    SomeNat (SomeNat),
    cmpNat,
    sameNat,
    type (+),
    type (-),
  )
import GHC.TypeNats (natVal, sameNat, someNatVal)
import Util.Index (next, prev)
import Util.Index qualified as Index
import Util.LinAlg ( Line, Point2D(..), Y(..), X(..), add, scalarMult, dist )
import System.Random (Random)
import Prelude hiding ((++))

type Circular i = Vector i Point2D

newtype Radius = Radius {unRadius :: Double}
  deriving newtype (Show, Eq, Num, Ord, Fractional, FromJSON, ToJSON)


circularGraph :: forall n. (KnownNat n) => X -> Y -> Radius -> Circular n
circularGraph (X centerx) (Y centery) (Radius radius) =
  generate $ \i -> Point2D (xCoord i) (yCoord i)
  where
    xCoord i = X (centerx + radius * cos (2 * pi * fromIntegral (getFinite i) / fromIntegral (natVal (Proxy :: Proxy n))))
    yCoord i = Y (centery + radius * sin (2 * pi * fromIntegral (getFinite i) / fromIntegral (natVal (Proxy :: Proxy n))))

area :: forall n. (KnownNat n) => Circular n -> Double
area v =
  V.foldl'
    ( \acc i ->
        acc
          + unX (v `index` i).x * unY ((v `index` next i).y - (v `index` prev i).y)
    )
    0.0
    indices
  where
    indices :: Vector n (Finite n)
    indices = generate id

toCircularLines :: (KnownNat n) => Circular n -> Vector n Line
toCircularLines ps = generate $ \i -> (ps `index` i, ps `index` Index.next i)

data AtLeastOneVector n rel
  = forall m mprev.
    (KnownNat m, m ~ mprev + 1, (rel m n)) =>
    AtLeastOneVector (Vector m Point2D)

deriving instance Show (AtLeastOneVector n rel)

class GtBy x n m
instance (n - m <= x) => GtBy x n m

class LtBy x n m
instance (m - n <= x) => LtBy x n m

maybeAddOnePoint ::
  forall n nprev.
  (KnownNat n) =>
  (n ~ nprev + 1) =>
  Double ->
  Circular n ->
  AtLeastOneVector n (GtBy 1) -- could parametrize if we want to add several at once
maybeAddOnePoint threshold v =
  case V.findIndex (\(p, idx) -> dist p (v `index` Index.next idx) > threshold) (V.zip v $ V.generate id) of
    Just idx -> case someNatVal . fromInteger . getFinite $ idx of
      (SomeNat (_ :: Proxy i)) -> case ( sameNat (Proxy @((i + 1) + (n - (i + 1)))) (Proxy @n),
                                         cmpNat (Proxy @i) (Proxy @n)
                                       ) of
        (Just Refl, LTI) ->
          let (p1, p2) = (v `index` idx, v `index` Index.next idx)
              midpoint = p1 `add` p2 `scalarMult` 0.5
              v' :: Vector (n + 1) Point2D
              v' = V.take @(i + 1) v ++ V.cons midpoint (V.drop @(i + 1) v)
           in AtLeastOneVector v'
        _ -> error "impossible"
    Nothing -> AtLeastOneVector v

data Exists n = forall m. m + 1 ~ n => Exists

maybeRemoveOnePoint ::
  forall n nprev.
  (KnownNat n) =>
  (n ~ nprev + 1) =>
  Double ->
  Circular n ->
  AtLeastOneVector n (LtBy 1) -- could parametrize if we want to remove several at once
maybeRemoveOnePoint threshold v =
  case V.findIndex (\(p, idx) -> dist p (v `index` Index.prev idx) < threshold) (V.zip v $ V.generate id) of
    Just idx -> case someNatVal . fromInteger . getFinite $ idx of
      (SomeNat (_ :: Proxy i)) -> case ( cmpNat (Proxy @n) (Proxy @1), -- needed to guarantee the result is still non-empty
                                         sameNat (Proxy @(i + (n - i))) (Proxy @n), -- needed for take
                                         sameNat (Proxy @((i + 1) + (n - (i + 1)))) (Proxy @n), -- needed for drop
                                         cmpNat (Proxy @i) (Proxy @n) -- needed to guarantee we can take
                                       ) of
        (EQI, _, _, _) -> AtLeastOneVector v
        (GTI, Just Refl, Just Refl, LTI) ->
          let (p1, p2) = (v `index` Index.prev idx, v `index` idx)
              midpoint = p1 `add` p2 `scalarMult` 0.5
              -- Auxiliary proofs
              less :: i < n => n - (i + 1) :~: n - i - 1
              less = Refl
              eq :: i < n => i + (n - i - 1) :~: n - 1
              eq = Refl
              greaterthan1 :: forall m. n > 1 => Exists nprev
              greaterthan1 = Exists @nprev @(nprev - 1)
              -- First the modified existing index
              v' = v // [(Index.prev . finite . fromIntegral $ natVal (Proxy @i), midpoint)]
              -- Then simplified vector lengths
              v'' :: Vector i Point2D
              v'' = V.take @i v'
              v''' :: Vector (n - i - 1) Point2D 
              v''' = case eq of 
                Refl -> V.drop @(i + 1) v'
              v'''' :: Vector nprev Point2D
              v'''' = case Refl of
                Refl -> v'' ++ v'''
           -- Finally we bring into scope evidence
           -- that there exists an m such that nprev = m + 1
           in case greaterthan1 of
            Exists -> AtLeastOneVector v''''
        _ -> error ("impossible: " <> show (natVal $ Proxy @i) <> ", " <> show (natVal $ Proxy @nprev) <> ", ")
    Nothing -> AtLeastOneVector v
