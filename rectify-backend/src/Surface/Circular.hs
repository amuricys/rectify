{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}

module Surface.Circular where

import Data.Aeson (FromJSON, ToJSON)
import Data.Finite (Finite, getFinite)
import Data.Finite.Integral (weaken)
import Data.Proxy (Proxy (..))
import Data.Singletons   -- SNat, Sing, withSomeSing
import Data.Singletons.Sigma      -- Σ-type (:@:) and (:&:)
import Data.Type.Equality ((:~:) (Refl))
import Data.Vector qualified as Unsized
import Data.Vector.Sized (Vector, generate, index, (++))
import Data.Vector.Sized qualified as V
import Debug.Pretty.Simple (pTraceShow)
import GHC.TypeLits
  ( KnownNat,
    SomeNat (SomeNat),
    type (+),
    type (-), Nat,
  )
import GHC.TypeNats (natVal, sameNat, someNatVal)
import Surface.Index (next, prev)
import Surface.Index qualified as Index
import Surface.LinAlg (Line, Point2D (..), X (..), Y (..), add, dist, scalarMult)
import System.Random (Random)
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding ((++))
import Prelude.Singletons
import Data.Kind (Type)

type Circular i = Vector i Point2D

newtype Radius = Radius {unRadius :: Double}
  deriving newtype (Show, Eq, Num, Fractional, FromJSON, ToJSON)

newtype Compression = Compression {unCompression :: Double}
  deriving newtype (Show, Eq, Num, Ord, Fractional, FromJSON, ToJSON, Random)

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

addPoint :: forall i n m. (KnownNat n, KnownNat i, n ~ i + m) => Point2D -> Circular n -> Circular (1 + n)
addPoint p g = convert $ V.take @i g ++ V.cons p (V.drop @i g)
  where
    -- Valid because of the constraint n ~ i + m. Just to avoid algebraic manipulation stuff
    convert :: Circular (i + (1 + m)) -> Circular (1 + n)
    convert = unsafeCoerce

toCircularLines :: (KnownNat n) => Vector n Point2D -> Vector n Line
toCircularLines ps = generate $ \i -> (ps `index` i, ps `index` Index.next i)

-- The defunctionalized version, so we can hand it to `Sigma`:
type VecSym0 (p :: Type) n = Vector n p

maybeAddOnePoint ::
  forall n nprev.
  (KnownNat n) =>
  (n ~ nprev + 1) =>
  Double ->
  Vector n Point2D ->
  Sigma Nat (TyCon1 (FlipSym Vector) Point2D)
maybeAddOnePoint threshold v =
  case V.findIndex (\(p, i) -> dist p (v `index` Index.next i) > threshold) (V.zip v $ V.generate id) of
    Just i -> case someNatVal . fromInteger . getFinite $ i of
      (SomeNat (_ :: Proxy actuali)) -> case sameNat (Proxy @((actuali + 1) + (n - (actuali + 1)))) (Proxy @n) of
        Just Refl ->
          let (p1, p2) = (v `index` i, v `index` Index.next i)
           in Left . convert $
                V.take @(actuali + 1) v ++ V.cons (p1 `add` p2 `scalarMult` 0.5) (V.drop @(actuali + 1) v)
          where
            convert :: Circular ((actuali + 1) + (1 + (n - (actuali + 1)))) -> Circular (n + 1)
            convert = unsafeCoerce
        _ -> error "impossible"
    Nothing -> Right v