module Rito.Mesh (mesh, mesh', Mesh, dmesh, emesh, fmesh) where

import Prelude

import Control.Plus (empty)
import Data.Foldable (oneOf)
import Data.Variant (Variant, match)
import FRP.Event (Event, bang, makeEvent, subscribe)
import Rito.Core as C
import Rito.Euler (Euler)
import Rito.Matrix4 (Matrix4)
import Rito.Quaternion (Quaternion)
import Rito.Threeful (Child, DynamicChildren(..), Eventful(..), FixedChildren(..), Threeful(..), handleKids)
import Rito.Vector3 (Vector3)

emesh
  :: forall lock payload. Event (Threeful C.Mesh lock payload) -> Threeful C.Mesh lock payload
emesh = Eventful' <<< Eventful

fmesh
  :: forall lock payload. Array (Threeful C.Mesh lock payload) -> Threeful C.Mesh lock payload
fmesh = FixedChildren' <<< FixedChildren

dmesh
  :: forall lock payload
   . Event (Event (Child C.Mesh lock payload))
  -> Threeful C.Mesh lock payload
dmesh = DynamicChildren' <<< DynamicChildren

----
newtype Mesh = Mesh
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

mesh'
  :: forall lock payload
   . C.Geometry lock payload
  -> C.Material lock payload
  -> Event Mesh
  -> Threeful C.Mesh lock payload
  -> Threeful C.Mesh lock payload
mesh' (C.Geometry geo) (C.Material mat) props kidz = PlainOld' $ C.Mesh go
  where
  go
    parent
    di@
      ( C.ThreeInterpret
          { ids
          , deleteFromCache
          , makeMesh
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
        [ bang $ makeMesh
            { id: me
            , parent: parent.parent
            , scope: parent.scope
            }
        , geo
            { parent: me
            , scope: parent.scope
            , raiseId: mempty
            }
            di
        , mat
            { parent: me
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
                  , scale: \{ x, y, z } -> setScale { id: me, x, y, z }
                  , lookAt: setLookAt <<< { id: me, v: _ }
                  }
            )
        , handleKids me parent.scope di kidz
        ]

-- group
--   :: forall i lock payload
--    . Event Group
--   -> Threeful lock payload
--   -> Threeful lock payload
-- group props kidz = C.Geometry go
--   where
--   go
--     parent
--     di@( C.ThreeInterpret
--         { ids
--         , createMesh
--         , deleteFromCache
--         , makeMesh
--         , connectGeometry
--         , connectMaterial
--         }
--     ) = makeEvent \k -> do
--     me <- ids
--     parent.raiseId me
--     -- do geo
--     -- do mat
--     map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
--       oneOf
--         [ bang $ makeMesh
--             { id: me
--             , parent: parent.parent
--             , scope: parent.scope
--             , geometry: ?hole
--             , material: ?hole
--             }
--         , __internalRitoFlatten me di kidz
--         ]

mesh
  :: forall lock payload
   . C.Geometry lock payload
  -> C.Material lock payload
  -> Event Mesh
  -> Threeful C.Mesh lock payload
mesh geo mat props = mesh' geo mat props (Eventful' (Eventful empty))