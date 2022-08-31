module Rito.RoundRobin
  ( InstanceId
  , Instance(..)
  , singleInstance
  , roundRobinInstancedMesh
  , Semaphore(..)
  ) where

import Prelude

import Bolson.EffectFn.Core as Bolson
import Control.Plus (empty)
import Data.Array (nub, uncons, (..))
import Data.Foldable (oneOf, traverse_)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Variant (Variant, match)
import Effect (Effect)
import Effect.Ref as Ref
import FRP.Event.EffectFn (Event, makeEvent, subscribe)

import Foreign.Object as Object
import Record (union)
import Rito.Color (Color)
import Rito.Core as C
import Rito.Matrix4 (Matrix4)
import Rito.THREE as THREE
import Web.TouchEvent (Touch, TouchEvent)
import Web.UIEvent.MouseEvent (MouseEvent)

newtype Instance = Instance
  ( Variant
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
      , matrix4 :: Matrix4
      , color :: Color
      )
  )

derive instance Newtype Instance _

newtype InstanceId = InstanceId { meshId :: String, instanceId :: Int }

withRemoval'
  :: forall a. Ref.Ref (Object.Object a) -> String -> a -> a -> Effect a
withRemoval' p s attach remove = do
  Ref.modify_ (Object.insert s remove) p
  pure attach

singleInstance
  :: forall lock payload
   . Event Instance
  -> InstanceId
  -> C.Instance lock payload
singleInstance props (InstanceId { meshId, instanceId }) = C.Instance
  go
  where
  go
    ( C.ThreeInterpret
        { setSingleInstancedMeshMatrix4
        , setSingleInstancedMeshColor
        , setIMOnClick
        , setIMOnMouseDown
        , setIMOnMouseUp
        , setIMOnMouseMove
        , setIMOnTouchStart
        , setIMOnTouchEnd
        , setIMOnTouchMove
        , setIMOnTouchCancel
        , removeIMOnClick
        , removeIMOnMouseDown
        , removeIMOnMouseUp
        , removeIMOnMouseMove
        , removeIMOnTouchStart
        , removeIMOnTouchEnd
        , removeIMOnTouchMove
        , removeIMOnTouchCancel
        }
    ) = makeEvent \k -> do

    u <- flip subscribe k $ oneOf
      [ makeEvent \pusher -> do
          unsubs <- Ref.new Object.empty
          let withRemoval = withRemoval' unsubs
          usu <- subscribe props \(Instance msh) -> pusher =<<
            ( msh # match
                { matrix4: \matrix4 -> pure $ setSingleInstancedMeshMatrix4 $
                    { id: meshId
                    , instanceId
                    , matrix4
                    }
                , color: \color -> pure $ setSingleInstancedMeshColor $
                    { id: meshId
                    , instanceId
                    , color
                    }
                , onClick: \onClick -> withRemoval "click"
                    (setIMOnClick { id: meshId, instanceId, onClick })
                    (removeIMOnClick { id: meshId, instanceId, onClick })
                , onMouseDown: \onMouseDown -> withRemoval "mousedown"
                    ( setIMOnMouseDown
                        { id: meshId, instanceId, onMouseDown }
                    )
                    ( removeIMOnMouseDown
                        { id: meshId, instanceId, onMouseDown }
                    )
                , onMouseUp: \onMouseUp -> withRemoval "mouseup"
                    ( setIMOnMouseUp
                        { id: meshId, instanceId, onMouseUp }
                    )
                    ( removeIMOnMouseUp
                        { id: meshId, instanceId, onMouseUp }
                    )
                , onMouseMove: \onMouseMove -> withRemoval "mousemove"
                    ( setIMOnMouseMove
                        { id: meshId, instanceId, onMouseMove }
                    )
                    ( removeIMOnMouseMove
                        { id: meshId, instanceId, onMouseMove }
                    )
                , onTouchStart: \onTouchStart -> withRemoval "touchstart"
                    ( setIMOnTouchStart
                        { id: meshId, instanceId, onTouchStart }
                    )
                    ( removeIMOnTouchStart
                        { id: meshId, instanceId, onTouchStart }
                    )
                , onTouchEnd: \onTouchEnd -> withRemoval "touchend"
                    ( setIMOnTouchEnd
                        { id: meshId, instanceId, onTouchEnd }
                    )
                    ( removeIMOnTouchEnd
                        { id: meshId, instanceId, onTouchEnd }
                    )
                , onTouchMove: \onTouchMove -> withRemoval "touchmove"
                    ( setIMOnTouchMove
                        { id: meshId, instanceId, onTouchMove }
                    )
                    ( removeIMOnTouchMove
                        { id: meshId, instanceId, onTouchMove }
                    )
                , onTouchCancel: \onTouchCancel -> withRemoval "touchcancel"
                    ( setIMOnTouchCancel
                        { id: meshId, instanceId, onTouchCancel }
                    )
                    ( removeIMOnTouchCancel
                        { id: meshId, instanceId, onTouchCancel }
                    )
                }
            )
          pure
            do
              removes <- Ref.read unsubs
              traverse_ pusher removes
              usu

      ]
    pure u
data Stage = Begin | Middle | End

data Semaphore a = Acquire a | Release

roundRobinInstancedMesh
  :: forall lock payload
   . { matrix4 :: THREE.TMatrix4
     , mesh :: THREE.TMesh
     , instancedMesh :: THREE.TInstancedMesh
     }
  -> Int
  -> C.Geometry lock payload
  -> C.Material lock payload
  -> Event
       (Event (Semaphore (InstanceId -> C.Instance lock payload)))
  -> C.AMesh lock payload
roundRobinInstancedMesh mmi count (C.Geometry geo) (C.Material mat) props =
  Bolson.Element' $ C.Mesh go
  where
  go
    parent
    di@
      ( C.ThreeInterpret
          { ids
          , deleteFromCache
          , makeInstancedMesh
          }
      ) = makeEvent \topK -> do
    me <- ids
    geoR <- Ref.new Nothing
    matR <- Ref.new Nothing
    parent.raiseId me
    u0 <- flip subscribe topK $
      oneOf
        [ geo
            -- we set the parent to nothing
            -- because we need to attribute the parent
            -- in makeInstancedMesh
            { parent: Nothing
            , scope: parent.scope
            , raiseId: \id -> Ref.write (Just id) geoR
            }
            di
        , mat
            { parent: Nothing
            , scope: parent.scope
            , raiseId: \id -> Ref.write (Just id) matR
            }
            di
        ]
    geoId <- Ref.read geoR
    matId <- Ref.read matR
    u1 <- flip subscribe topK $ case geoId, matId of
      Nothing, _ -> empty
      _, Nothing -> empty
      Just gid, Just mid -> oneOf
        [ pure $ makeInstancedMesh
            ( { id: me
              , parent: parent.parent
              , scope: parent.scope
              , geometry: gid
              , material: mid
              , count
              } `union` mmi
            )
        -- todo: instanced logic is copied a fair bit from bolson's flatten
        -- but it's different enough that it's tough to merge it with flatten
        -- insert instanced logic here
        , makeEvent \k -> do
            available <- Ref.new (0 .. (count - 1))
            cancelInner <- Ref.new Object.empty
            cancelOuter <-
              -- each child gets its own scope
              subscribe props \inner ->
                do
                  -- holds the previous id
                  availableNow <- Ref.read available
                  let currentHead = uncons availableNow
                  case currentHead of
                    Nothing -> pure unit
                    Just { head, tail } -> do
                      Ref.write tail available
                      myUnsubId <- ids
                      myUnsub <- Ref.new (pure unit)
                      eltsUnsubId <- ids
                      eltsUnsub <- Ref.new (pure unit)
                      myImmediateCancellation <- Ref.new (pure unit)
                      stageRef <- Ref.new Begin
                      c0 <- subscribe inner \kid' -> do
                        stage <- Ref.read stageRef
                        case kid', stage of
                          Release, Middle -> do
                            Ref.write End stageRef
                            -- we may need to run the cancellation twice
                            -- because the refs are not populated yet if it runs
                            -- immediately
                            -- thus the `nub` below
                            let
                              mic =
                                -- use nub so that the operation is idempotent
                                -- as we may call it twice
                                -- if the cancelation is immediate
                                Ref.modify_ ((_ <> [ head ]) >>> nub) available
                                  *> join (Ref.read myUnsub)
                                  *> join (Ref.read eltsUnsub)
                                  *> Ref.modify_
                                    (Object.delete myUnsubId)
                                    cancelInner
                                  *> Ref.modify_
                                    (Object.delete eltsUnsubId)
                                    cancelInner

                            (Ref.write mic myImmediateCancellation) *> mic
                          Acquire kid, Begin -> do
                            -- holds the current id
                            Ref.write Middle stageRef
                            c1 <- subscribe
                              ( ( (\(C.Instance i) -> i) $ kid
                                    ( InstanceId
                                        { meshId: me, instanceId: head }
                                    )
                                ) di
                              )
                              k
                            Ref.modify_
                              (Object.insert eltsUnsubId c1)
                              cancelInner
                            Ref.write c1 eltsUnsub
                          _, _ -> pure unit
                      Ref.write c0 myUnsub
                      Ref.modify_ (Object.insert myUnsubId c0)
                        cancelInner
                      join (Ref.read myImmediateCancellation)
            pure do
              Ref.read cancelInner >>= traverse_ identity
              cancelOuter
        ]
    pure (topK (deleteFromCache { id: me }) *> u0 *> u1)