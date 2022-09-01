module Rito.Mesh (mesh, mesh', Mesh(..), Mesh') where

import Prelude

import Bolson.Control (flatten)
import Bolson.Core (fixed)
import Bolson.Core as Bolson
import Control.Monad.ST.Global as Region
import Control.Monad.ST.Internal (ST)
import Control.Monad.ST.Internal as Ref
import Data.Foldable (oneOf)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Variant (Variant, match)
import Effect (Effect)
import FRP.Event (Event, makeLemmingEvent)
import Foreign.Object (Object, values)
import Foreign.Object as Object
import Heterogeneous.Mapping (class Mapping, hmap)
import Record (union)
import Rito.Core as C
import Rito.ST.ForEach (foreachST)
import Rito.THREE as THREE
import Web.TouchEvent (Touch, TouchEvent)
import Web.UIEvent.MouseEvent (MouseEvent)

----
type Mesh' = Variant
  ( onClick :: MouseEvent -> Effect Unit
  , onMouseDown :: MouseEvent -> Effect (MouseEvent -> Effect Unit)
  , onMouseUp :: MouseEvent -> Effect Unit
  , onMouseMove :: MouseEvent -> Effect Unit
  , onTouchStart ::
      Touch
      -> Effect
           { end :: TouchEvent -> Effect Unit
           , cancel :: TouchEvent -> Effect Unit
           }
  , onTouchEnd :: Touch -> Effect Unit
  , onTouchMove :: Touch -> Effect Unit
  , onTouchCancel :: Touch -> Effect Unit
  | C.Object3D
  )

newtype Mesh = Mesh Mesh'

instance Newtype Mesh Mesh'

data MakeEvent = MakeEvent

instance Mapping MakeEvent (x -> a) (x -> ST Region.Global a) where
  mapping MakeEvent = map pure

withRemoval' :: forall a. Ref.STRef Region.Global (Object a) -> String -> a -> a -> ST Region.Global a
withRemoval' p s attach remove = do
  void $ Ref.modify (Object.insert s remove) p
  pure attach

mesh'
  :: forall lock payload
   . { mesh :: THREE.TMesh }
  -> C.Geometry lock payload
  -> C.Material lock payload
  -> Event Mesh
  -> Array (C.AMesh lock payload)
  -> C.AMesh lock payload
mesh' mshhhh (C.Geometry geo) (C.Material mat) props kidz = Bolson.Element' $
  C.Mesh go
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
      ) = makeLemmingEvent \mySub k -> do
    me <- ids
    parent.raiseId me
    unsub <- mySub
      ( oneOf
          [ pure $ makeMesh
              { id: me
              , parent: parent.parent
              , scope: parent.scope
              , mesh: mshhhh.mesh
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
          , makeLemmingEvent \mySub pusher -> do
              unsubs <- Ref.new Object.empty
              let withRemoval = withRemoval' unsubs
              usu <- mySub props \(Mesh msh) -> pusher =<<
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
                        , onTouchStart: \onTouchStart -> withRemoval
                            "touchstart"
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
                foreachST (values removes) pusher
                usu
          , flatten
              { doLogic: absurd
              , ids: unwrap >>> _.ids
              , disconnectElement: unwrap >>> _.disconnect
              , toElt: \(C.Mesh obj) -> Bolson.Element obj
              }
              { parent: Just me, scope: parent.scope, raiseId: \_ -> pure unit }
              di
              (fixed kidz)
          ]
      )
      k
    pure do
      k (deleteFromCache { id: me })
      unsub

mesh
  :: forall lock payload
   . { mesh :: THREE.TMesh }
  -> C.Geometry lock payload
  -> C.Material lock payload
  -> Event Mesh
  -> C.AMesh lock payload
mesh msh geo mat props = mesh' msh geo mat props []