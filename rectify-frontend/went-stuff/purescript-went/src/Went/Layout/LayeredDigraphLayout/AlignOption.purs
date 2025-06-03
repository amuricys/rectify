module Went.Layout.LayeredDigraphLayout.AlignOption where

import Prelude

import Data.Newtype (class Newtype)
import GoJS.Diagram.Layout.Constants (alignAll_, alignLowerLeft_, alignLowerRight_, alignNone_, alignUpperLeft_, alignUpperRight_)

-- These option types and their semigroup instances are here to emulate the Sankey's use of || in
-- this way: go.LayeredDigraphLayout.PackStraighten || go.LayeredDigraphLayout.PackMedian. I don't know
-- why that flavor of flag-setting makes any sense in gojs (it's just evaluating to the first nonzero of those,
-- and the library obviously knows what it is) but I'm just emulating the behavior. These numbers
-- are wrapped in newtypes so that only the newtyped constants are usable - they're essentially smart constructors.


newtype AlignOption = AlignOption Number

alignNone :: AlignOption
alignNone = AlignOption alignNone_

alignUpperLeft :: AlignOption
alignUpperLeft = AlignOption alignUpperLeft_

alignUpperRight :: AlignOption
alignUpperRight = AlignOption alignUpperRight_

alignLowerLeft :: AlignOption
alignLowerLeft = AlignOption alignLowerLeft_

alignLowerRight :: AlignOption
alignLowerRight = AlignOption alignLowerRight_

alignAll :: AlignOption
alignAll = AlignOption alignAll_

instance Semigroup AlignOption where
  append (AlignOption p1) (AlignOption p2) = AlignOption $ if p1 == 0.0 then p2 else p1

derive instance Newtype AlignOption _