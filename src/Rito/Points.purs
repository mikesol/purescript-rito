module Rito.Points (points, points', Points(..), Points') where

import Prelude

import Bolson.EffectFn.Control (flatten)
import Bolson.EffectFn.Core (fixed)
import Bolson.EffectFn.Core as Bolson
import Data.Foldable (oneOf, traverse_)
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
type Points' = Variant
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

newtype Points = Points Points'

instance Newtype Points Points'

data MakeEvent = MakeEvent

instance Mapping MakeEvent (x -> a) (x -> Effect a) where
  mapping MakeEvent = map pure

withRemoval' :: forall a. Ref.Ref (Object a) -> String -> a -> a -> Effect a
withRemoval' p s attach remove = do
  Ref.modify_ (Object.insert s remove) p
  pure attach

points'
  :: forall lock payload
   . { points :: THREE.TPoints }
  -> C.Geometry lock payload
  -> C.Material lock payload
  -> Event Points
  -> Array (C.APoints lock payload)
  -> C.APoints lock payload
points' mshhhh (C.Geometry geo) (C.Material mat) props kidz = Bolson.Element' $ C.Points go
  where
  go
    parent
    di@
      ( C.ThreeInterpret
          { ids
          , deleteFromCache
          , makePoints
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
        [ pure $ makePoints
            { id: me
            , parent: parent.parent
            , scope: parent.scope
            , points: mshhhh.points
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
            usu <- subscribe props \(Points msh) -> pusher =<<
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
            , toElt: \(C.Points obj) -> Bolson.Element obj
            }
            { parent: Just me, scope: parent.scope, raiseId: pure mempty }
            di
            (fixed kidz)
        ]

points
  :: forall lock payload
   . { points :: THREE.TPoints }
  -> C.Geometry lock payload
  -> C.Material lock payload
  -> Event Points
  -> C.APoints lock payload
points msh geo mat props = points' msh geo mat props []