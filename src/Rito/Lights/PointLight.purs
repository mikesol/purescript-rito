module Rito.Lights.PointLight
  ( pointLight
  , pointLight_
  , PointLight(..)
  , PointLight'
  , class InitialPointLight
  , PointLightOptions
  , toInitializePointLight
  ) where

import Prelude

import Bolson.Core as Bolson
import Control.Alt ((<|>))
import Control.Plus (empty)
import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Data.Newtype (class Newtype)
import Data.Variant (Variant, match)
import FRP.Event (Event, bang, makeEvent, subscribe)
import Record (union)
import Rito.Color (class ColorRepresentation, Color, color)
import Rito.Core as C

data PointLightOptions = PointLightOptions

instance
  ColorRepresentation n =>
  ConvertOption PointLightOptions
    "color"
    n
    Color where
  convertOption _ _ = color

instance
  ConvertOption PointLightOptions
    "intensity"
    Number
    Number where
  convertOption _ _ = identity

instance
  ConvertOption PointLightOptions
    "distance"
    Number
    Number where
  convertOption _ _ = identity

instance
  ConvertOption PointLightOptions
    "decay"
    Number
    Number where
  convertOption _ _ = identity

type PointLightOptional =
  ( color :: Color
  , intensity :: Number
  , distance :: Number
  , decay :: Number
  )

type PointLightAll =
  (| PointLightOptional)

defaultPointLight :: { | PointLightOptional }
defaultPointLight =
  { color: color 0xffffff
  , intensity: 1.0
  , distance: 0.0
  , decay: 1.0
  }

class InitialPointLight i where
  toInitializePointLight :: i -> C.InitializePointLight

instance InitialPointLight C.InitializePointLight where
  toInitializePointLight = identity

instance
  ConvertOptionsWithDefaults PointLightOptions { | PointLightOptional }
    { | provided }
    { | PointLightAll } =>
  InitialPointLight { | provided } where
  toInitializePointLight provided = C.InitializePointLight
    (convertOptionsWithDefaults PointLightOptions defaultPointLight provided)

type PointLight' = Variant
  ( color :: Color
  , intensity :: Number
  , distance :: Number
  , decay :: Number
  | C.Object3D
  )

newtype PointLight = PointLight PointLight'
instance Newtype PointLight PointLight'

pointLight
  :: forall i lock payload
   . InitialPointLight i
  => i
  -> Event PointLight
  -> C.ALight lock payload
pointLight i' atts = Bolson.Element' $ C.Light go
  where
  C.InitializePointLight i = toInitializePointLight i'
  go
    parent
    di@( C.ThreeInterpret
        { ids
        , deleteFromCache
        , makePointLight
        , setColor
        , setIntensity
        , setDistance
        , setDecay
        }
    ) = makeEvent \k -> do
    me <- ids
    parent.raiseId me
    map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
      bang
        ( makePointLight
            { id: me
            , parent: parent.parent
            , scope: parent.scope
            , color: i.color
            , intensity: i.intensity
            , distance: i.distance
            , decay: i.decay
            }
        )
        <|>
          ( map
              ( \(PointLight e) -> match
                  (union { color: setColor <<< { id: me, color: _ }
                  , intensity: setIntensity <<< { id: me, intensity: _ }
                  , distance: setDistance <<< { id: me, distance: _ }
                  , decay: setDecay <<< { id: me, decay: _ }
                  } (C.object3D me di))
                  e
              )
              atts
          )

pointLight_
  :: forall i lock payload
   . InitialPointLight i
  => i
  -> C.ALight lock payload
pointLight_ i = pointLight i empty