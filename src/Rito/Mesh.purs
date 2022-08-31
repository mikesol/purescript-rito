module Rito.Mesh (mesh, mesh', Mesh(..), Mesh') where

import Prelude

import Bolson.EffectFn.Control (flatten)
import Bolson.EffectFn.Core (fixed)
import Bolson.EffectFn.Core as Bolson
import Data.Foldable (oneOf)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Variant (Variant, match)
import Effect (Effect, foreachE)
import Effect.Ref as Ref
import FRP.Event.EffectFn (Event, makeEvent, subscribe)
import Foreign.Object (Object, values)
import Foreign.Object as Object
import Heterogeneous.Mapping (class Mapping, hmap)
import Record (union)
import Rito.Core as C
import Rito.THREE as THREE
import Web.TouchEvent (Touch, TouchEvent)
import Web.UIEvent.MouseEvent (MouseEvent)

----
type Mesh' = Variant
  ( onClick :: MouseEvent -> Effect Unit
  , onMouseDown :: MouseEvent -> Effect (MouseEvent -> Effect Unit)
  , onMouseUp :: MouseEvent -> Effect Unit
  , onMouseMove :: MouseEvent -> Effect Unit
  , onTouchStart :: Touch -> Effect { end :: TouchEvent -> Effect Unit, cancel :: TouchEvent -> Effect Unit }
  , onTouchEnd :: Touch -> Effect Unit
  , onTouchMove :: Touch -> Effect Unit
  , onTouchCancel :: Touch -> Effect Unit
  | C.Object3D
  )

newtype Mesh = Mesh Mesh'

instance Newtype Mesh Mesh'

data MakeEvent = MakeEvent

instance Mapping MakeEvent (x -> a) (x -> Effect a) where
  mapping MakeEvent = map pure

withRemoval' :: forall a. Ref.Ref (Object a) -> String -> a -> a -> Effect a
withRemoval' p s attach remove = do
  Ref.modify_ (Object.insert s remove) p
  pure attach

mesh'
  :: forall lock payload
   . { mesh :: THREE.TMesh }
  -> C.Geometry lock payload
  -> C.Material lock payload
  -> Event Mesh
  -> Array (C.AMesh lock payload)
  -> C.AMesh lock payload
mesh' mshhhh (C.Geometry geo) (C.Material mat) props kidz = Bolson.Element' $ C.Mesh go
  where
  go
    parent
    di@
      ( C.ThreeInterpret
          { ids
          , deleteFromCache
          , makeMesh
          , setOnClick
          , setOnMouseDown
          , setOnMouseUp
          , setOnMouseMove
          , setOnTouchStart
          , setOnTouchEnd
          , setOnTouchMove
          , setOnTouchCancel
          , removeOnClick
          , removeOnMouseDown
          , removeOnMouseUp
          , removeOnMouseMove
          , removeOnTouchStart
          , removeOnTouchEnd
          , removeOnTouchMove
          , removeOnTouchCancel
          }
      ) = makeEvent \k -> do
    me <- ids
    parent.raiseId me
    map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
      oneOf
        [ pure $ makeMesh
            { id: me
            , parent: parent.parent
            , scope: parent.scope
            , mesh: mshhhh.mesh
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
        , makeEvent \pusher -> do
            unsubs <- Ref.new Object.empty
            let withRemoval = withRemoval' unsubs
            usu <- subscribe props \(Mesh msh) -> pusher =<<
              ( msh # match
                  ( union
                      { onClick: \onClick -> withRemoval "click"
                          (setOnClick { id: me, onClick })
                          (removeOnClick { id: me, onClick })
                      , onMouseDown: \onMouseDown -> withRemoval "mousedown"
                          ( setOnMouseDown
                              { id: me, onMouseDown }
                          )
                          ( removeOnMouseDown
                              { id: me, onMouseDown }
                          )
                      , onMouseUp: \onMouseUp -> withRemoval "mouseup"
                          ( setOnMouseUp
                              { id: me, onMouseUp }
                          )
                          ( removeOnMouseUp
                              { id: me, onMouseUp }
                          )
                      , onMouseMove: \onMouseMove -> withRemoval "mousemove"
                          ( setOnMouseMove
                              { id: me, onMouseMove }
                          )
                          ( removeOnMouseMove
                              { id: me, onMouseMove }
                          )
                      , onTouchStart: \onTouchStart -> withRemoval "touchstart"
                          ( setOnTouchStart
                              { id: me, onTouchStart }
                          )
                          ( removeOnTouchStart
                              { id: me, onTouchStart }
                          )
                      , onTouchEnd: \onTouchEnd -> withRemoval "touchend"
                          ( setOnTouchEnd
                              { id: me, onTouchEnd }
                          )
                          ( removeOnTouchEnd
                              { id: me, onTouchEnd }
                          )
                      , onTouchMove: \onTouchMove -> withRemoval "touchmove"
                          ( setOnTouchMove
                              { id: me, onTouchMove }
                          )
                          ( removeOnTouchMove
                              { id: me, onTouchMove }
                          )
                      , onTouchCancel: \onTouchCancel -> withRemoval
                          "touchcancel"
                          ( setOnTouchCancel
                              { id: me, onTouchCancel }
                          )
                          ( removeOnTouchCancel
                              { id: me, onTouchCancel }
                          )
                      }
                      (hmap MakeEvent (C.object3D me di))
                  )
              )
            pure do
              removes <- Ref.read unsubs
              foreachE (values removes) pusher
              usu
        , flatten
            { doLogic: absurd
            , ids: unwrap >>> _.ids
            , disconnectElement: unwrap >>> _.disconnect
            , toElt: \(C.Mesh obj) -> Bolson.Element obj
            }
            { parent: Just me, scope: parent.scope, raiseId: pure mempty }
            di
            (fixed kidz)
        ]

mesh
  :: forall lock payload
   . { mesh :: THREE.TMesh }
  -> C.Geometry lock payload
  -> C.Material lock payload
  -> Event Mesh
  -> C.AMesh lock payload
mesh msh geo mat props = mesh' msh geo mat props []