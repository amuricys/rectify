module Went.Layout.LayeredDigraphLayout.PackOption where

import Prelude

import Data.Newtype (class Newtype)
import GoJS.Diagram.Layout.Constants (packAll_, packExpand_, packMedian_, packNone_, packStraighten_)

-- These option types and their semigroup instances are here to emulate the Sankey's use of || in
-- this way: go.LayeredDigraphLayout.PackStraighten || go.LayeredDigraphLayout.PackMedian. I don't know
-- why that flavor of flag-setting makes any sense in gojs (it's just evaluating to the first nonzero of those,
-- and the library obviously knows what it is) but I'm just emulating the behavior. These numbers
-- are wrapped in newtypes so that only the newtyped constants are usable - they're essentially smart constructors.

newtype PackOption = PackOption Number

packNone :: PackOption
packNone = PackOption packNone_

packExpand :: PackOption
packExpand = PackOption packExpand_

packStraighten :: PackOption
packStraighten = PackOption packStraighten_

packMedian :: PackOption
packMedian = PackOption packMedian_

packAll :: PackOption
packAll = PackOption packAll_

instance Semigroup PackOption where
  append (PackOption p1) (PackOption p2) = PackOption $ if p1 == 0.0 then p2 else p1

derive instance Newtype PackOption _