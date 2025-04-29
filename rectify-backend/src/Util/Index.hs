{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Util.Index where

import Prelude
import GHC.TypeLits
import Data.Aeson
import Data.Finite ( Finite, finite, getFinite )
import Data.Proxy
import Data.Functor.Contravariant (contramap)
import Data.Functor.Compose (Compose)

type Index i = Finite i

instance (KnownNat n) => FromJSON (Index n) where
  parseJSON = fmap finite . parseJSON

instance ToJSON (Index n) where
  toJSON = toJSON . getFinite

instance (KnownNat n) => FromJSONKey (Index n) where
  fromJSONKey = finite <$> fromJSONKey -- FromJSONKey is covariant

instance (KnownNat n) => ToJSONKey (Index n) where
  toJSONKey :: KnownNat n => ToJSONKeyFunction (Index n)
  toJSONKey = contramap getFinite toJSONKey -- ToJSONKey is contravariant

next :: forall n. KnownNat n => Finite n -> Finite n
next i = add i 1

prev :: forall n. KnownNat n => Finite n -> Finite n
prev i = sub i 1

add :: forall n. KnownNat n => Finite n -> Integer -> Finite n
add i j = finite $ (getFinite i + j) `mod` natVal (Proxy :: Proxy n)

sub :: forall n. KnownNat n => Finite n -> Integer -> Finite n
sub i j = finite $ (getFinite i - j) `mod` natVal (Proxy :: Proxy n)

distCtrclkwise :: forall otherCtrclkwise i. KnownNat otherCtrclkwise => KnownNat i => Finite i -> Integer
distCtrclkwise ind = let 
  otherCtrclkwise = natVal (Proxy :: Proxy otherCtrclkwise) 
  in
  if otherCtrclkwise > getFinite ind
    then getFinite ind + (natVal (Proxy :: Proxy i) - otherCtrclkwise)
    else getFinite ind - otherCtrclkwise

distClkwise :: forall otherClkwise i. KnownNat otherClkwise => KnownNat i => Finite i -> Integer
distClkwise ind = let
  otherClkwise = natVal (Proxy :: Proxy otherClkwise) 
  in
  if otherClkwise >= getFinite ind
    then otherClkwise - getFinite ind
    else natVal (Proxy :: Proxy i) + otherClkwise - getFinite ind
