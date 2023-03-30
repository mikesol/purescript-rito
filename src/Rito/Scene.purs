module Rito.Scene
  ( scene
  , class InitialScene
  , toInitializeScene
  , SceneOptions(..)
  , Scene(..)
  , Background(..)
  ) where

import Prelude

import Bolson.Control (flatten)
import Bolson.Core (fixed)
import Bolson.Core as Bolson
import Control.Monad.ST.Uncurried (mkSTFn2, runSTFn1, runSTFn2)
import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Data.Foldable (oneOf)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Variant (Variant, match)
import FRP.Event (Event, Subscriber(..), makeLemmingEventO)
import Record (union)
import Rito.Color as Col
import Rito.Core (FogInfo(..))
import Rito.Core as C
import Rito.CubeTexture as CT
import Rito.THREE as THREE
import Rito.Texture as T

data SceneOptions = SceneOptions

instance
  ConvertOption SceneOptions
    "scene"
    THREE.TScene
    THREE.TScene where
  convertOption _ _ = identity

instance
  ConvertOption SceneOptions
    "fog"
    C.FogInfo
    (Maybe C.FogInfo) where
  convertOption _ _ = Just

type SceneOptional =
  ( fog :: Maybe C.FogInfo
  )

type SceneAll =
  (scene :: THREE.TScene | SceneOptional)

defaultScene :: { | SceneOptional }
defaultScene =
  { fog: Nothing }

class InitialScene i where
  toInitializeScene :: i -> C.InitializeScene

instance InitialScene C.InitializeScene where
  toInitializeScene = identity

instance
  ConvertOptionsWithDefaults SceneOptions
    { | SceneOptional }
    { | provided }
    { | SceneAll } =>
  InitialScene { | provided } where
  toInitializeScene provided = C.InitializeScene
    ( convertOptionsWithDefaults SceneOptions
        defaultScene
        provided
    )

data Background
  = CubeTexture CT.CubeTexture
  | Texture T.Texture
  | Color Col.Color

newtype Scene = Scene
  (Variant (background :: Background | C.Object3D))

derive instance Newtype Scene _

scene
  :: forall i payload
   . InitialScene i
  => i
  -> Event Scene
  -> Array (C.ASceneful payload)
  -> C.Scene payload
scene ctor' props kidz = C.Scene go
  where
  (C.InitializeScene ctor) = toInitializeScene ctor'
  go
    parent
    di@
      ( C.ThreeInterpret
          { ids
          , deleteFromCache
          , makeScene
          , setBackgroundCubeTexture
          , setBackgroundTexture
          , setBackgroundColor
          }
      ) = makeLemmingEventO $ mkSTFn2 \(Subscriber mySub) k -> do
    me <- ids
    parent.raiseId me
    let
      myFog = ctor.fog <#> case _ of
        FogExp2Info r -> r
    unsub <- runSTFn2 mySub
      ( oneOf
          [ pure $ makeScene
              { id: me
              , parent: parent.parent
              , scope: parent.scope
              , scene: ctor.scene
              , fog: myFog
              }
          , props <#>
              ( \(Scene msh) ->
                  msh # match
                    ( union
                        { background: case _ of
                            CubeTexture ct -> setBackgroundCubeTexture
                              { id: me, cubeTexture: ct }
                            Texture ct -> setBackgroundTexture
                              { id: me, texture: ct }
                            Color ct -> setBackgroundColor { id: me, color: ct }
                        }
                        (C.object3D me di)
                    )
              )
          , flatten
              { doLogic: absurd
              , ids: unwrap >>> _.ids
              , disconnectElement: unwrap >>> _.disconnect
              , toElt: \(C.Sceneful obj) -> Bolson.Element obj
              }
              { parent: Just me, scope: parent.scope, raiseId: \_ -> pure unit }
              di
              (fixed kidz)
          ]
      )
      k
    pure do
      runSTFn1 k (deleteFromCache { id: me })
      unsub
