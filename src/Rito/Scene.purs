module Rito.Scene (scene, Scene, fscene) where

import Prelude

import Data.Foldable (oneOf)
import Data.Variant (Variant, match)
import FRP.Event (Event, bang, makeEvent, subscribe)
import Rito.Core as C
import Rito.Euler (Euler)
import Rito.Matrix4 (Matrix4)
import Rito.Quaternion (Quaternion)
import Rito.Threeful (Child, DynamicChildren(..), Eventful(..), FixedChildren(..), Threeful(..), handleKids)
import Rito.Vector3 (Vector3)

-- escene
--   :: forall lock payload. Event (Threeful C.Sceneful lock payload) -> Threeful C.Scene lock payload
-- escene = Eventful' <<< Eventful

fscene
  :: forall lock payload
   . Event Scene
  -> Array (Threeful C.Sceneful lock payload)
  -> C.Scene lock payload
fscene e a = scene e (FixedChildren' (FixedChildren a))

-- dscene
--   :: forall lock payload
--    . Event (Event (Child C.Scene lock payload))
--   -> Threeful C.Scene lock payload
-- dscene = DynamicChildren' <<< DynamicChildren

----
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
  -> Threeful C.Sceneful lock payload
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
        , handleKids me parent.scope di kidz
        ]
