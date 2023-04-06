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

data AmbientLightOptions = AmbientLightOptions

instance
  ConvertOption AmbientLightOptions
    "ambientLight"
    THREE.TAmbientLight
    THREE.TAmbientLight where
  convertOption _ _ = identity

instance
  ConvertOption AmbientLightOptions
    "color"
    Color
    Color where
  convertOption _ _ = identity

instance
  ConvertOption AmbientLightOptions
    "intensity"
    Number
    Number where
  convertOption _ _ = identity

type AmbientLightOptional =
  ( intensity :: Number
  )

type AmbientLightAll =
  ( color :: Color
  , ambientLight :: THREE.TAmbientLight
  | AmbientLightOptional
  )

defaultAmbientLight :: { | AmbientLightOptional }
defaultAmbientLight =
  { intensity: 1.0
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
    ( convertOptionsWithDefaults AmbientLightOptions defaultAmbientLight
        provided
    )

type AmbientLight' = Variant
  ( color :: Color
  , intensity :: Number
  -- object3D
  | C.Object3D
  )
newtype AmbientLight = AmbientLight AmbientLight'
instance Newtype AmbientLight AmbientLight'

ambientLight
  :: forall i payload
   . InitialAmbientLight i
  => i
  -> Event AmbientLight
  -> C.ALight payload
ambientLight i' atts = Bolson.Element' $ C.Light go
  where
  C.InitializeAmbientLight i = toInitializeAmbientLight i'
  go
    parent
    di@
      ( C.ThreeInterpret
          { ids
          , deleteFromCache
          , makeAmbientLight
          , setColor
          , setIntensity
          }
      ) = makeLemmingEventO $ mkSTFn2 \(Subscriber mySub) k -> do
    me <- ids
    parent.raiseId me
    unsub <- runSTFn2 mySub
      ( oneOf
          [ pure
              ( makeAmbientLight
                  { id: me
                  , parent: parent.parent
                  , scope: parent.scope
                  , ambientLight: i.ambientLight
                  , color: i.color
                  , intensity: i.intensity
                  }
              )
          , map
              ( \(AmbientLight e) -> match
                  ( union
                      { color: setColor <<< { id: me, color: _ }
                      , intensity: setIntensity <<< { id: me, intensity: _ }
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

ambientLight_
  :: forall i payload
   . InitialAmbientLight i
  => i
  -> C.ALight payload
ambientLight_ i = ambientLight i empty