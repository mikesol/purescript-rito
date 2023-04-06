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

data DirectionalLightOptions = DirectionalLightOptions

instance
  ConvertOption DirectionalLightOptions
    "directionalLight"
    THREE.TDirectionalLight
    THREE.TDirectionalLight where
  convertOption _ _ = identity

instance
  ConvertOption DirectionalLightOptions
    "color"
    Color
    Color where
  convertOption _ _ = identity

instance
  ConvertOption DirectionalLightOptions
    "intensity"
    Number
    Number where
  convertOption _ _ = identity

type DirectionalLightOptional =
  ( intensity :: Number
  )

type DirectionalLightAll =
  ( color :: Color
  , directionalLight :: THREE.TDirectionalLight
  | DirectionalLightOptional
  )

defaultDirectionalLight :: { | DirectionalLightOptional }
defaultDirectionalLight =
  { intensity: 1.0
  }

class InitialDirectionalLight i where
  toInitializeDirectionalLight :: i -> C.InitializeDirectionalLight

instance InitialDirectionalLight C.InitializeDirectionalLight where
  toInitializeDirectionalLight = identity

instance
  ConvertOptionsWithDefaults DirectionalLightOptions
    { | DirectionalLightOptional }
    { | provided }
    { | DirectionalLightAll } =>
  InitialDirectionalLight { | provided } where
  toInitializeDirectionalLight provided = C.InitializeDirectionalLight
    ( convertOptionsWithDefaults DirectionalLightOptions defaultDirectionalLight
        provided
    )

type DirectionalLight' = Variant
  ( color :: Color
  , intensity :: Number
  | C.Object3D
  )
newtype DirectionalLight = DirectionalLight DirectionalLight'
instance Newtype DirectionalLight DirectionalLight'

directionalLight
  :: forall i payload
   . InitialDirectionalLight i
  => i
  -> Event DirectionalLight
  -> C.ALight payload
directionalLight i' atts = Bolson.Element' $ C.Light go
  where
  C.InitializeDirectionalLight i = toInitializeDirectionalLight i'
  go
    parent
    di@
      ( C.ThreeInterpret
          { ids
          , deleteFromCache
          , makeDirectionalLight
          , setColor
          , setIntensity
          }
      ) = makeLemmingEventO $ mkSTFn2 \(Subscriber mySub) k -> do
    me <- ids
    parent.raiseId me
    unsub <- runSTFn2 mySub
      ( oneOf
          [ pure
              ( makeDirectionalLight
                  { id: me
                  , parent: parent.parent
                  , scope: parent.scope
                  , directionalLight: i.directionalLight
                  , color: i.color
                  , intensity: i.intensity
                  }
              )
          , map
              ( \(DirectionalLight e) -> match
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

directionalLight_
  :: forall i payload
   . InitialDirectionalLight i
  => i
  -> C.ALight payload
directionalLight_ i = directionalLight i empty