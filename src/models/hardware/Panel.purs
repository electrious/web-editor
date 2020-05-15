module Model.Hardware.Panel where

import Prelude

import Data.Lens ((^.))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Editor.Common.Lenses (_id)
import Foreign.Generic (class Decode, class Encode, defaultOptions, genericDecode, genericEncode)
import Model.Hardware.PanelType (PanelType(..))

newtype Panel = Panel {
    id                                     :: Int,
    created_at                             :: String,
    updated_at                             :: String,
    name                                   :: String,
    power                                  :: Number,
    manufacturer                           :: String,
    active                                 :: Boolean,
    default                                :: Boolean,
    datasheet                              :: String,
    bipv                                   :: Boolean,
    size_short                             :: Number,
    size_long                              :: Number,
    nominal_operating_cell_temperature     :: Number,
    area                                   :: Number,
    series_cells_count                     :: Int,
    short_circuit_current                  :: Number,
    open_circuit_voltage                   :: Number,
    max_power_current                      :: Number,
    max_power_voltage                      :: Number,
    short_circuit_current_temperature_coef :: Number,
    open_circuit_voltage_temperature_coef  :: Number,
    diode_ideality_factor                  :: Number,
    photocurrent                           :: Number,
    diode_reverse_saturation_current       :: Number,
    series_resistance                      :: Number,
    shunt_resistance                       :: Number,
    power_temperature_coefficient          :: Number,
    adjust                                 :: Number,
    version                                :: String,
    ptc                                    :: Number,
    technology                             :: String,
    year_approved_at                       :: Int
}

derive instance newtypePanel :: Newtype Panel _
derive instance genericPanel :: Generic Panel _
instance showPanel :: Show Panel where
    show = genericShow
instance encodePanel :: Encode Panel where
    encode = genericEncode (defaultOptions { unwrapSingleConstructors = true })
instance decodePanel :: Decode Panel where
    decode = genericDecode (defaultOptions { unwrapSingleConstructors = true })

getPanelType :: Panel -> PanelType
getPanelType p | p ^. _id == 1 = Premium
               | p ^. _id == 2 = Standard
               | otherwise     = Premium
