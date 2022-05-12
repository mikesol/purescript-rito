module Rito.Lights.DirectionalLight
  ( directionalLight
  , directionalLight_
  , DirectionalLight(..)
  , DirectionalLight'
  , class InitialDirectionalLight
  , DirectionalLightOptions
  , toInitializeDirectionalLight
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

data DirectionalLightOptions = DirectionalLightOptions

instance
  ColorRepresentation n =>
  ConvertOption DirectionalLightOptions
    "color"
    n
    Color where
  convertOption _ _ = color

instance
  ConvertOption DirectionalLightOptions
    "intensity"
    Number
    Number where
  convertOption _ _ = identity

type DirectionalLightOptional =
  ( color :: Color
  , intensity :: Number
  )

type DirectionalLightAll =
  (| DirectionalLightOptional)

defaultDirectionalLight :: { | DirectionalLightOptional }
defaultDirectionalLight =
  { color: color 0xffffff
  , intensity: 1.0
  }

class InitialDirectionalLight i where
  toInitializeDirectionalLight :: i -> C.InitializeDirectionalLight

instance InitialDirectionalLight C.InitializeDirectionalLight where
  toInitializeDirectionalLight = identity

instance
  ConvertOptionsWithDefaults DirectionalLightOptions { | DirectionalLightOptional }
    { | provided }
    { | DirectionalLightAll } =>
  InitialDirectionalLight { | provided } where
  toInitializeDirectionalLight provided = C.InitializeDirectionalLight
    (convertOptionsWithDefaults DirectionalLightOptions defaultDirectionalLight provided)

type DirectionalLight' = Variant
  ( color :: Color
  , intensity :: Number
  | C.Object3D
  )
newtype DirectionalLight = DirectionalLight DirectionalLight'
instance Newtype DirectionalLight DirectionalLight'

directionalLight
  :: forall i lock payload
   . InitialDirectionalLight i
  => i
  -> Event DirectionalLight
  -> C.ALight lock payload
directionalLight i' atts = Bolson.Element' $ C.Light go
  where
  C.InitializeDirectionalLight i = toInitializeDirectionalLight i'
  go
    parent
    di@( C.ThreeInterpret
        { ids
        , deleteFromCache
        , makeDirectionalLight
        , setColor
        , setIntensity
        }
    ) = makeEvent \k -> do
    me <- ids
    parent.raiseId me
    map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
      bang
        ( makeDirectionalLight
            { id: me
            , parent: parent.parent
            , scope: parent.scope
            , color: i.color
            , intensity: i.intensity
            }
        )
        <|>
          ( map
              ( \(DirectionalLight e) -> match
                  (union { color: setColor <<< { id: me, color: _ }
                  , intensity: setIntensity <<< { id: me, intensity: _ }
                  } (C.object3D me di))
                  e
              )
              atts
          )

directionalLight_
  :: forall i lock payload
   . InitialDirectionalLight i
  => i
  -> C.ALight lock payload
directionalLight_ i = directionalLight i empty