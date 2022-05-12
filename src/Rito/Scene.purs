module Rito.Scene (scene, Scene) where

import Prelude

import Bolson.Control (flatten)
import Bolson.Core (Entity(..), fixed)
import Bolson.Core as Bolson
import Control.Plus (empty)
import Data.Foldable (oneOf)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Variant (Variant, match)
import FRP.Event (Event, bang, makeEvent, subscribe)
import Rito.Core as C
import Rito.Euler (Euler)
import Rito.Group as Group
import Rito.Matrix4 (Matrix4)
import Rito.Quaternion (Quaternion)
import Rito.Vector3 (Vector3)
import Safe.Coerce (coerce)
import Unsafe.Coerce (unsafeCoerce)

newtype Scene = Scene
  ( Variant
      ( matrix4 :: Matrix4
      , quaternion :: Quaternion
      , rotationFromAxisAngle :: { axis :: Vector3, angle :: Number }
      , rotationFromEuler :: Euler
      , rotationFromMatrix :: Matrix4
      , rotationFromQuaternion :: Quaternion
      , rotateOnAxis :: { axis :: Vector3, angle :: Number }
      , rotateOnWorldAxis :: { axis :: Vector3, angle :: Number }
      , rotateX :: Number
      , rotateY :: Number
      , rotateZ :: Number
      , translateOnAxis :: { axis :: Vector3, distance :: Number }
      , translateX :: Number
      , translateY :: Number
      , translateZ :: Number
      , scale :: { x :: Number, y :: Number, z :: Number }
      , lookAt :: Vector3
      )
  )

scene
  :: forall lock payload
   . Event Scene
  -> Array (C.ASceneful lock payload)
  -> C.AScene lock payload
scene props kidz = Element' $ C.Scene go
  where
  go
    parent
    di@
      ( C.ThreeInterpret
          { ids
          , deleteFromCache
          , makeScene
          , setMatrix4
          , setQuaternion
          , setRotationFromAxisAngle
          , setRotationFromEuler
          , setRotationFromMatrix
          , setRotationFromQuaternion
          , setRotateOnAxis
          , setRotateOnWorldAxis
          , setRotateX
          , setRotateY
          , setRotateZ
          , setTranslateOnAxis
          , setTranslateX
          , setTranslateY
          , setTranslateZ
          , setScale
          , setLookAt
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
                  { matrix4: setMatrix4 <<< { id: me, matrix4: _ }
                  , quaternion: setQuaternion <<< { id: me, quaternion: _ }
                  , rotationFromAxisAngle: \{ axis, angle } ->
                      setRotationFromAxisAngle { id: me, axis, angle }
                  , rotationFromEuler: setRotationFromEuler <<<
                      { id: me, euler: _ }
                  , rotationFromMatrix: setRotationFromMatrix <<<
                      { id: me, matrix4: _ }
                  , rotationFromQuaternion: setRotationFromQuaternion <<<
                      { id: me, quaternion: _ }
                  , rotateOnAxis: \{ axis, angle } -> setRotateOnAxis
                      { id: me, axis, angle }
                  , rotateOnWorldAxis: \{ axis, angle } -> setRotateOnWorldAxis
                      { id: me, axis, angle }
                  , rotateX: setRotateX <<< { id: me, rotateX: _ }
                  , rotateY: setRotateY <<< { id: me, rotateY: _ }
                  , rotateZ: setRotateZ <<< { id: me, rotateZ: _ }
                  , translateOnAxis: \{ axis, distance } -> setTranslateOnAxis
                      { id: me, axis, distance }
                  , translateX: setTranslateX <<< { id: me, translateX: _ }
                  , translateY: setTranslateY <<< { id: me, translateY: _ }
                  , translateZ: setTranslateZ <<< { id: me, translateZ: _ }
                  , scale: \{ x, y, z } -> setScale { id: me, x, y, z }
                  , lookAt: setLookAt <<< { id: me, v: _ }
                  }
            )
        , flatten
            { doLogic: absurd
            , ids: unwrap >>> _.ids
            , disconnectElement: unwrap >>> _.disconnect
            , wrapElt: \a -> (unsafeCoerce :: C.AGroup lock payload -> C.AScene lock payload) (Group.group empty [ coerce a ])
            , toElt: \(C.Scene obj) -> Bolson.Element obj
            }
            { parent: Just me, scope: parent.scope, raiseId: pure mempty }
            di
            ( fixed
                ( map
                    ( unsafeCoerce
                        :: C.ASceneful lock payload
                        -> C.AScene lock payload
                    )
                    kidz
                )
            )
        ]
