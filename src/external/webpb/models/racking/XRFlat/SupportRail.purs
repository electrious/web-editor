module WebPB.Models.Racking.XRFlat.SupportRail where

import Effect (Effect)
import WebPB.Class (class HasLength, class HasPBUUID, class IsPBArrayComp)

foreign import data SupportRailPB :: Type
foreign import mkSupportRailPB :: Effect SupportRailPB

instance hasPBUUIDSupportRailPB :: HasPBUUID SupportRailPB
instance isPBArrayCompSupportRailPB :: IsPBArrayComp SupportRailPB
instance hasLengthSupportRailPB :: HasLength SupportRailPB
