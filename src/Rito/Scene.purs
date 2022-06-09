module Rito.Scene (scene, Scene, Background(..)) where

import Prelude

import Bolson.Control (flatten)
import Bolson.Core (fixed)
import Bolson.Core as Bolson
import Data.Foldable (oneOf)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Variant (Variant, match)
import FRP.Event (Event, bang, makeEvent, subscribe)
import Record (union)
import Rito.Color as Col
import Rito.Core as C
import Rito.CubeTexture as CT
import Rito.Texture as T

data Background
  = CubeTexture CT.CubeTexture
  | Texture T.Texture
  | Color Col.Color

newtype Scene = Scene
  (Variant (background :: Background | C.Object3D))

scene
  :: forall lock payload
   . Event Scene
  -> Array (C.ASceneful lock payload)
  -> C.Scene lock payload
scene props kidz = C.Scene go
  where
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
      ) = makeEvent \k -> do
    me <- ids
    parent.raiseId me
    map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
      oneOf
        [ bang $ makeScene
            { id: me
            , parent: parent.parent
            , scope: parent.scope
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
            { parent: Just me, scope: parent.scope, raiseId: pure mempty }
            di
            (fixed kidz)
        ]
