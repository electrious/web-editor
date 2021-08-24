module Model.Hardware.Panel where

import Prelude

import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, (:=), (~>))
import Data.Generic.Rep (class Generic)
import Data.Lens ((^.))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
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

derive instance Newtype Panel _
derive instance Generic Panel _
instance Show Panel where
    show = genericShow
instance Encode Panel where
    encode = genericEncode (defaultOptions { unwrapSingleConstructors = true })
instance Decode Panel where
    decode = genericDecode (defaultOptions { unwrapSingleConstructors = true })
instance EncodeJson Panel where
    encodeJson (Panel p) = "id" := p.id
                        ~> "created_at" := p.created_at
                        ~> "updated_at" := p.updated_at
                        ~> "name" := p.name
                        ~> "power" := p.power
                        ~> "manufacturer" := p.manufacturer
                        ~> "active" := p.active
                        ~> "default" := p.default
                        ~> "datasheet" := p.datasheet
                        ~> "bipv" := p.bipv
                        ~> "size_short" := p.size_short
                        ~> "size_long" := p.size_long
                        ~> "nominal_operating_cell_temperature" := p.nominal_operating_cell_temperature
                        ~> "area" := p.area
                        ~> "series_cells_count" := p.series_cells_count
                        ~> "short_circuit_current" := p.short_circuit_current
                        ~> "open_circuit_voltage" := p.open_circuit_voltage
                        ~> "max_power_current" := p.max_power_current
                        ~> "max_power_voltage" := p.max_power_voltage
                        ~> "short_circuit_current_temperature_coef" := p.short_circuit_current_temperature_coef
                        ~> "open_circuit_voltage_temperature_coef" := p.open_circuit_voltage_temperature_coef
                        ~> "diode_ideality_factor" := p.diode_ideality_factor
                        ~> "photocurrent" := p.photocurrent
                        ~> "diode_reverse_saturation_current" := p.diode_reverse_saturation_current
                        ~> "series_resistance" := p.series_resistance
                        ~> "shunt_resistance" := p.shunt_resistance
                        ~> "power_temperature_coefficient" := p.power_temperature_coefficient
                        ~> "adjust" := p.adjust
                        ~> "version" := p.version
                        ~> "ptc" := p.ptc
                        ~> "technology" := p.technology
                        ~> "year_approved_at" := p.year_approved_at
                        ~> jsonEmptyObject
instance DecodeJson Panel where
    decodeJson = decodeJson >=> f
        where f o = mkPanel <$> o .: "id"
                            <*> o .: "created_at"
                            <*> o .: "updated_at"
                            <*> o .: "name"
                            <*> o .: "power"
                            <*> o .: "manufacturer"
                            <*> o .: "active"
                            <*> o .: "default"
                            <*> o .: "datasheet"
                            <*> o .: "bipv"
                            <*> o .: "size_short"
                            <*> o .: "size_long"
                            <*> o .: "nominal_operating_cell_temperature"
                            <*> o .: "area"
                            <*> o .: "series_cells_count"
                            <*> o .: "short_circuit_current"
                            <*> o .: "open_circuit_voltage"
                            <*> o .: "max_power_current"
                            <*> o .: "max_power_voltage"
                            <*> o .: "short_circuit_current_temperature_coef"
                            <*> o .: "open_circuit_voltage_temperature_coef"
                            <*> o .: "diode_ideality_factor"
                            <*> o .: "photocurrent"
                            <*> o .: "diode_reverse_saturation_current"
                            <*> o .: "series_resistance"
                            <*> o .: "shunt_resistance"
                            <*> o .: "power_temperature_coefficient"
                            <*> o .: "adjust"
                            <*> o .: "version"
                            <*> o .: "ptc"
                            <*> o .: "technology"
                            <*> o .: "year_approved_at"
                            
mkPanel :: Int -> String -> String -> String -> Number -> String -> Boolean -> Boolean -> String -> Boolean -> Number -> Number -> Number -> Number -> Int -> Number -> Number -> Number -> Number -> Number -> Number -> Number -> Number -> Number -> Number -> Number -> Number -> Number -> String -> Number -> String -> Int -> Panel
mkPanel id created_at updated_at name power manufacturer active default datasheet bipv size_short size_long nominal_operating_cell_temperature area series_cells_count short_circuit_current open_circuit_voltage max_power_current max_power_voltage short_circuit_current_temperature_coef open_circuit_voltage_temperature_coef diode_ideality_factor photocurrent diode_reverse_saturation_current series_resistance shunt_resistance power_temperature_coefficient adjust version ptc technology year_approved_at = Panel {
    id : id,
    created_at : created_at,
    updated_at : updated_at,
    name : name,
    power : power,
    manufacturer : manufacturer,
    active : active,
    default : default,
    datasheet : datasheet,
    bipv : bipv,
    size_short : size_short,
    size_long : size_long,
    nominal_operating_cell_temperature : nominal_operating_cell_temperature,
    area : area,
    series_cells_count : series_cells_count,
    short_circuit_current : short_circuit_current,
    open_circuit_voltage : open_circuit_voltage,
    max_power_current : max_power_current,
    max_power_voltage : max_power_voltage,
    short_circuit_current_temperature_coef : short_circuit_current_temperature_coef,
    open_circuit_voltage_temperature_coef : open_circuit_voltage_temperature_coef,
    diode_ideality_factor : diode_ideality_factor,
    photocurrent : photocurrent,
    diode_reverse_saturation_current : diode_reverse_saturation_current,
    series_resistance : series_resistance,
    shunt_resistance : shunt_resistance,
    power_temperature_coefficient : power_temperature_coefficient,
    adjust : adjust,
    version : version,
    ptc : ptc,
    technology : technology,
    year_approved_at : year_approved_at
}

getPanelType :: Panel -> PanelType
getPanelType p | p ^. _id == 1 = Premium
               | p ^. _id == 2 = Standard
               | otherwise     = Premium
