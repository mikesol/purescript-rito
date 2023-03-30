module Rito.Points
  ( Points(..)
  , points
  , points'
  , Points'
  ) where

import Prelude

import Bolson.Control (flatten)
import Bolson.Core (fixed)
import Bolson.Core as Bolson
import Control.Monad.ST.Global as Region
import Control.Monad.ST.Internal (ST)
import Control.Monad.ST.Internal as Ref
import Control.Monad.ST.Uncurried (mkSTFn1, mkSTFn2, runSTFn1, runSTFn2)
import Data.Foldable (oneOf)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Variant (Variant, match)
import Effect (Effect)
import FRP.Event (Event, Subscriber(..), makeLemmingEventO)
import Foreign.Object (Object, values)
import Foreign.Object as Object
import Record (union)
import Rito.Core as C
import Rito.ST.ForEach (foreachST)
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

withRemoval' :: forall a. Ref.STRef Region.Global (Object a) -> String -> a -> a -> ST Region.Global a
withRemoval' p s attach remove = do
  void $ Ref.modify (Object.insert s remove) p
  pure attach

points'
  :: forall payload
   . { points :: THREE.TPoints }
  -> C.Geometry payload
  -> C.Material payload
  -> Event Points
  -> Array (C.APoints payload)
  -> C.APoints payload
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
      ) = makeLemmingEventO $ mkSTFn2 \(Subscriber mySub0) k -> do
    me <- ids
    parent.raiseId me
    unsub <- runSTFn2 mySub0
      (oneOf
        [ pure $ makePoints
            { id: me
            , parent: parent.parent
            , scope: parent.scope
            , points: mshhhh.points
            }
        , geo
            { parent: Just me
            , scope: parent.scope
            , raiseId: \_ -> pure unit
            }
            di
        , mat
            { parent: Just me
            , scope: parent.scope
            , raiseId: \_ -> pure unit
            }
            di
        , makeLemmingEventO $ mkSTFn2 \(Subscriber mySub) pusher -> do
            unsubs <- Ref.new Object.empty
            let withRemoval = withRemoval' unsubs
            usu <- runSTFn2 mySub props $ mkSTFn1 \(Points msh) -> do
                x <- ( msh # match
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
                        (C.pureObject3D me di)
                    )
                )
                runSTFn1 pusher x
            pure do
              removes <- Ref.read unsubs
              foreachST (values removes) \i -> runSTFn1 pusher i
              usu
        , flatten
            { doLogic: absurd
            , ids: unwrap >>> _.ids
            , disconnectElement: unwrap >>> _.disconnect
            , toElt: \(C.Points obj) -> Bolson.Element obj
            }
            { parent: Just me, scope: parent.scope, raiseId: \_ -> pure unit }
            di
            (fixed kidz)
        ]) k
    pure do
      runSTFn1 k (deleteFromCache { id: me })
      unsub

points
  :: forall payload
   . { points :: THREE.TPoints }
  -> C.Geometry payload
  -> C.Material payload
  -> Event Points
  -> C.APoints payload
points msh geo mat props = points' msh geo mat props []