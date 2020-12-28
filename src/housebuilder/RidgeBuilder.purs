module HouseBuilder.RidgeBuilder where


import Prelude

import Control.Plus (empty)
import Data.Newtype (class Newtype)
import FRP.Dynamic (Dynamic)
import FRP.Event (Event)
import Model.ActiveMode (ActiveMode)
import Model.HouseBuilder.FloorPlan (FloorPlan)
import Rendering.Node (Node)


-- state of adding ridge point
data RidgeState = RidgeAdding
                | RidgeAdded





newtype RidgeEditorConf = RidgeEditorConf {
    floor :: FloorPlan,
    mode  :: Dynamic ActiveMode
    }

derive instance newtypeRidgeEditorConf :: Newtype RidgeEditorConf _

editRidges :: forall e. RidgeEditorConf -> Node e (Event Unit)
editRidges conf = do
    
    pure empty
