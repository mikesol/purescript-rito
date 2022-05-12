module Rito.Lights.AmbientLight
  ( ambientLight
  , ambientLight_
  , AmbientLight(..)
  , AmbientLight'
  , class InitialAmbientLight
  , AmbientLightOptions
  , toInitializeAmbientLight
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

data AmbientLightOptions = AmbientLightOptions

instance
  ColorRepresentation n =>
  ConvertOption AmbientLightOptions
    "color"
    n
    Color where
  convertOption _ _ = color

instance
  ConvertOption AmbientLightOptions
    "intensity"
    Number
    Number where
  convertOption _ _ = identity

type AmbientLightOptional =
  ( color :: Color
  , intensity :: Number
  )

type AmbientLightAll =
  (| AmbientLightOptional)

defaultAmbientLight :: { | AmbientLightOptional }
defaultAmbientLight =
  { color: color 0xffffff
  , intensity: 1.0
  }

class InitialAmbientLight i where
  toInitializeAmbientLight :: i -> C.InitializeAmbientLight

instance InitialAmbientLight C.InitializeAmbientLight where
  toInitializeAmbientLight = identity

instance
  ConvertOptionsWithDefaults AmbientLightOptions { | AmbientLightOptional }
    { | provided }
    { | AmbientLightAll } =>
  InitialAmbientLight { | provided } where
  toInitializeAmbientLight provided = C.InitializeAmbientLight
    (convertOptionsWithDefaults AmbientLightOptions defaultAmbientLight provided)

type AmbientLight' = Variant
  ( color :: Color
  , intensity :: Number
  -- object3D
  | C.Object3D
  )
newtype AmbientLight = AmbientLight AmbientLight'
instance Newtype AmbientLight AmbientLight'

ambientLight
  :: forall i lock payload
   . InitialAmbientLight i
  => i
  -> Event AmbientLight
  -> C.ALight lock payload
ambientLight i' atts = Bolson.Element' $ C.Light go
  where
  C.InitializeAmbientLight i = toInitializeAmbientLight i'
  go
    parent
    di@( C.ThreeInterpret
        { ids
        , deleteFromCache
        , makeAmbientLight
        , setColor
        , setIntensity
        }
    ) = makeEvent \k -> do
    me <- ids
    parent.raiseId me
    map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
      bang
        ( makeAmbientLight
            { id: me
            , parent: parent.parent
            , scope: parent.scope
            , color: i.color
            , intensity: i.intensity
            }
        )
        <|>
          ( map
              ( \(AmbientLight e) -> match
                  (union { color: setColor <<< { id: me, color: _ }
                  , intensity: setIntensity <<< { id: me, intensity: _ }
                  } (C.object3D me di))
                  e
              )
              atts
          )

ambientLight_
  :: forall i lock payload
   . InitialAmbientLight i
  => i
  -> C.ALight lock payload
ambientLight_ i = ambientLight i empty