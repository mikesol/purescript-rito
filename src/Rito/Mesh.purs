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
import Effect (Effect)
import FRP.Event (Event, bang, keepLatest, makeEvent, subscribe)
import Heterogeneous.Mapping (class Mapping, hmap)
import Record (union)
import Rito.Core (toGroup)
import Rito.Core as C
import Rito.Group as Group
import Unsafe.Coerce (unsafeCoerce)
import Web.TouchEvent (Touch)
import Web.UIEvent.MouseEvent (MouseEvent)

----
type Mesh' = Variant
  ( onClick :: MouseEvent -> Effect Unit
  , onMouseDown :: MouseEvent -> Effect Unit
  , onMouseUp :: MouseEvent -> Effect Unit
  , onMouseMove :: MouseEvent -> Effect Unit
  , onTouchStart :: Touch -> Effect Unit
  , onTouchEnd :: Touch -> Effect Unit
  , onTouchMove :: Touch -> Effect Unit
  , onTouchCancel :: Touch -> Effect Unit
  | C.Object3D
  )
newtype Mesh = Mesh Mesh'
instance Newtype Mesh Mesh'

data MakeEvent = MakeEvent

instance Mapping MakeEvent (x -> a) (x -> Event a) where
  mapping MakeEvent = map bang

withRemoval :: forall a. a -> a -> Event a
withRemoval attach remove = makeEvent \k -> do
  k attach
  pure (k remove)

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
          , removeOnTouchCancel          }
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
        , keepLatest $ props <#>
            ( \(Mesh msh) ->
                msh # match
                  ( union
                      { onClick: \onClick -> withRemoval (setOnClick { id: me, onClick }) (removeOnClick { id: me, onClick })
                      , onMouseDown: \onMouseDown -> withRemoval (setOnMouseDown
                          { id: me, onMouseDown }) (removeOnMouseDown
                          { id: me, onMouseDown })
                      , onMouseUp: \onMouseUp -> withRemoval (setOnMouseUp
                          { id: me, onMouseUp }) (removeOnMouseUp
                          { id: me, onMouseUp })
                      , onMouseMove: \onMouseMove -> withRemoval (setOnMouseMove
                          { id: me, onMouseMove }) (removeOnMouseMove
                          { id: me, onMouseMove })
                      , onTouchStart: \onTouchStart -> withRemoval (setOnTouchStart
                          { id: me, onTouchStart }) (removeOnTouchStart
                          { id: me, onTouchStart })
                      , onTouchEnd: \onTouchEnd -> withRemoval (setOnTouchEnd
                          { id: me, onTouchEnd }) (removeOnTouchEnd
                          { id: me, onTouchEnd })
                      , onTouchMove: \onTouchMove -> withRemoval (setOnTouchMove
                          { id: me, onTouchMove }) (removeOnTouchMove
                          { id: me, onTouchMove })
                      , onTouchCancel: \onTouchCancel -> withRemoval (setOnTouchCancel
                          { id: me, onTouchCancel }) (removeOnTouchCancel
                          { id: me, onTouchCancel })
                      }
                      (hmap MakeEvent (C.object3D me di))
                  )
            )
        , flatten
            { doLogic: absurd
            , ids: unwrap >>> _.ids
            , disconnectElement: unwrap >>> _.disconnect
            , wrapElt: \a ->
                (unsafeCoerce :: C.AGroup lock payload -> C.AMesh lock payload)
                  (Group.group empty [ toGroup a ])
            , toElt: \(C.Mesh obj) -> Bolson.Element obj
            }
            { parent: Just me, scope: parent.scope, raiseId: pure mempty }
            di
            (fixed kidz)
        ]

mesh
  :: forall lock payload
   . C.Geometry lock payload
  -> C.Material lock payload
  -> Event Mesh
  -> C.AMesh lock payload
mesh geo mat props = mesh' geo mat props []