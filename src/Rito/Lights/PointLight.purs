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
import Control.Monad.ST.Uncurried (mkSTFn2, runSTFn1, runSTFn2)
import Control.Plus (empty)
import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Data.Foldable (oneOf)
import Data.Newtype (class Newtype)
import Data.Variant (Variant, match)
import FRP.Event (Event, Subscriber(..), makeLemmingEventO)
import Record (union)
import Rito.Color (Color)
import Rito.Core as C
import Rito.THREE as THREE

data PointLightOptions = PointLightOptions

instance
  ConvertOption PointLightOptions
    "pointLight"
    THREE.TPointLight
    THREE.TPointLight where
  convertOption _ _ = identity

instance
  ConvertOption PointLightOptions
    "color"
    Color
    Color where
  convertOption _ _ = identity

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
  ( intensity :: Number
  , distance :: Number
  , decay :: Number
  )

type PointLightAll =
  ( color :: Color
  , pointLight :: THREE.TPointLight
  | PointLightOptional
  )

defaultPointLight :: { | PointLightOptional }
defaultPointLight =
  { intensity: 1.0
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
  :: forall i payload
   . InitialPointLight i
  => i
  -> Event PointLight
  -> C.ALight payload
pointLight i' atts = Bolson.Element' $ C.Light go
  where
  C.InitializePointLight i = toInitializePointLight i'
  go
    parent
    di@
      ( C.ThreeInterpret
          { ids
          , deleteFromCache
          , makePointLight
          , setColor
          , setIntensity
          , setDistance
          , setDecay
          }
      ) = makeLemmingEventO $ mkSTFn2 \(Subscriber mySub) k -> do
    me <- ids
    parent.raiseId me
    unsub <- runSTFn2 mySub
      ( oneOf
          [ pure
              ( makePointLight
                  { id: me
                  , parent: parent.parent
                  , scope: parent.scope
                  , pointLight: i.pointLight
                  , color: i.color
                  , intensity: i.intensity
                  , distance: i.distance
                  , decay: i.decay
                  }
              )
          , map
              ( \(PointLight e) -> match
                  ( union
                      { color: setColor <<< { id: me, color: _ }
                      , intensity: setIntensity <<< { id: me, intensity: _ }
                      , distance: setDistance <<< { id: me, distance: _ }
                      , decay: setDecay <<< { id: me, decay: _ }
                      }
                      (C.object3D me di)
                  )
                  e
              )
              atts
          ]
      )
      k
    pure do
      runSTFn1 k (deleteFromCache { id: me })
      unsub

pointLight_
  :: forall i payload
   . InitialPointLight i
  => i
  -> C.ALight payload
pointLight_ i = pointLight i empty