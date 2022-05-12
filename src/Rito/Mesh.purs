module Rito.Mesh (mesh, mesh', Mesh(..), Mesh') where

import Prelude

import Bolson.Control (flatten)
import Bolson.Core (fixed)
import Bolson.Core as Bolson
import Control.Plus (empty)
import Data.Foldable (oneOf)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Variant (Variant, match)
import FRP.Event (Event, bang, makeEvent, subscribe)
import Rito.Core (toGroup)
import Rito.Core as C
import Rito.Euler (Euler)
import Rito.Group as Group
import Rito.Matrix4 (Matrix4)
import Rito.Quaternion (Quaternion)
import Rito.Vector3 (Vector3)
import Unsafe.Coerce (unsafeCoerce)

----
type Mesh' = Variant
  ( -- object3D
    matrix4 :: Matrix4
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
  , positionX :: Number
  , positionY :: Number
  , positionZ :: Number
  , scaleX :: Number
  , scaleY :: Number
  , scaleZ :: Number
  , lookAt :: Vector3
  )
newtype Mesh = Mesh Mesh'
instance Newtype Mesh Mesh'

mesh'
  :: forall lock payload
   . C.Geometry lock payload
  -> C.Material lock payload
  -> Event Mesh
  -> Array (C.AMesh lock payload)
  -> C.AMesh lock payload
mesh' (C.Geometry geo) (C.Material mat) props kidz = Bolson.Element' $ C.Mesh go
  where
  go
    parent
    di@
      ( C.ThreeInterpret
          { ids
          , deleteFromCache
          , makeMesh
          -- object 3D
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
          , setPositionX
          , setPositionY
          , setPositionZ
          , setScaleX
          , setScaleY
          , setScaleZ
          , setLookAt
          }
      ) = makeEvent \k -> do
    me <- ids
    parent.raiseId me
    map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
      oneOf
        [ bang $ makeMesh
            { id: me
            , parent: parent.parent
            , scope: parent.scope
            }
        , geo
            { parent: Just me
            , scope: parent.scope
            , raiseId: mempty
            }
            di
        , mat
            { parent: Just me
            , scope: parent.scope
            , raiseId: mempty
            }
            di
        , props <#>
            ( \(Mesh msh) ->
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
                  , positionX: setPositionX <<< { id: me, positionX: _ }
                  , positionY: setPositionY <<< { id: me, positionY: _ }
                  , positionZ: setPositionZ <<< { id: me, positionZ: _ }
                  , scaleX: setScaleX <<< { id: me, scaleX: _ }
                  , scaleY: setScaleY <<< { id: me, scaleY: _ }
                  , scaleZ: setScaleZ <<< { id: me, scaleZ: _ }
                  , lookAt: setLookAt <<< { id: me, v: _ }
                  }
            )
        , flatten
            { doLogic: absurd
            , ids: unwrap >>> _.ids
            , disconnectElement: unwrap >>> _.disconnect
            , wrapElt: \a -> (unsafeCoerce :: C.AGroup lock payload -> C.AMesh lock payload) (Group.group empty [ toGroup a ])
            , toElt: \(C.Mesh obj) -> Bolson.Element obj
            }
            { parent: Just me, scope: parent.scope, raiseId: pure mempty }
            di
            ( fixed kidz            )
        ]

mesh
  :: forall lock payload
   . C.Geometry lock payload
  -> C.Material lock payload
  -> Event Mesh
  -> C.AMesh lock payload
mesh geo mat props = mesh' geo mat props []