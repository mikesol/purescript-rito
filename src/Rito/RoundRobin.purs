module Rito.RoundRobin
  ( InstanceId
  , Instance(..)
  , singleInstance
  , roundRobinInstancedMesh
  , Semaphore(..)
  ) where

import Prelude

import Bolson.Core as Bolson
import Control.Monad.ST (ST)
import Control.Monad.ST.Global as Region
import Control.Monad.ST.Internal as Ref
import Control.Monad.ST.Uncurried (mkSTFn1, mkSTFn2, runSTFn1, runSTFn2)
import Control.Plus (empty)
import Data.Array (nub, uncons, (..))
import Data.Foldable (oneOf, traverse_)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Variant (Variant, match)
import Effect (Effect)
import FRP.Event (Event, Subscriber(..), makeLemmingEventO)
import Foreign.Object (values)
import Foreign.Object as Object
import Record (union)
import Rito.Color (Color)
import Rito.Core as C
import Rito.Matrix4 (Matrix4)
import Rito.ST.ForEach (foreachST)
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
  :: forall a
   . Ref.STRef Region.Global (Object.Object a)
  -> String
  -> a
  -> a
  -> ST Region.Global a
withRemoval' p s attach remove = do
  void $ Ref.modify (Object.insert s remove) p
  pure attach

singleInstance
  :: forall payload
   . Event Instance
  -> InstanceId
  -> C.Instance payload
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
    ) = makeLemmingEventO $ mkSTFn2 \(Subscriber mySub0) k -> do

    u <- runSTFn2 mySub0
      ( oneOf
          [ makeLemmingEventO $ mkSTFn2 \(Subscriber mySub) pusher -> do
              unsubs <- Ref.new Object.empty
              let withRemoval = withRemoval' unsubs
              usu <- runSTFn2 mySub props $ mkSTFn1 \(Instance msh) -> do
                x <-
                  ( msh # match
                      { matrix4: \matrix4 -> pure
                          $ setSingleInstancedMeshMatrix4
                          $
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
                      , onTouchCancel: \onTouchCancel -> withRemoval
                          "touchcancel"
                          ( setIMOnTouchCancel
                              { id: meshId, instanceId, onTouchCancel }
                          )
                          ( removeIMOnTouchCancel
                              { id: meshId, instanceId, onTouchCancel }
                          )
                      }
                  )
                runSTFn1 pusher x
              pure
                do
                  removes <- Ref.read unsubs
                  foreachST (values removes) \vr -> runSTFn1 pusher vr
                  usu

          ]
      )
      k
    pure u

data Stage = Begin | Middle | End

data Semaphore a = Acquire a | Release

roundRobinInstancedMesh
  :: forall payload
   . { matrix4 :: THREE.TMatrix4
     , mesh :: THREE.TMesh
     , instancedMesh :: THREE.TInstancedMesh
     }
  -> Int
  -> C.Geometry payload
  -> C.Material payload
  -> Event
       (Event (Semaphore (InstanceId -> C.Instance payload)))
  -> C.AMesh payload
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
      ) = makeLemmingEventO $ mkSTFn2 \(Subscriber mySub0) topK -> do
    me <- ids
    geoR <- Ref.new Nothing
    matR <- Ref.new Nothing
    parent.raiseId me
    u0 <- runSTFn2 mySub0
      ( oneOf
          [ geo
              -- we set the parent to nothing
              -- because we need to attribute the parent
              -- in makeInstancedMesh
              { parent: Nothing
              , scope: parent.scope
              , raiseId: \id -> void $ Ref.write (Just id) geoR
              }
              di
          , mat
              { parent: Nothing
              , scope: parent.scope
              , raiseId: \id -> void $ Ref.write (Just id) matR
              }
              di
          ]
      )
      topK
    geoId <- Ref.read geoR
    matId <- Ref.read matR
    u1 <- runSTFn2 mySub0
      ( case geoId, matId of
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
            , makeLemmingEventO $ mkSTFn2 \(Subscriber mySub) k -> do
                available <- Ref.new (0 .. (count - 1))
                cancelInner <- Ref.new Object.empty
                cancelOuter <-
                  -- each child gets its own scope
                  runSTFn2 mySub props $ mkSTFn1 \inner ->
                    do
                      -- holds the previous id
                      availableNow <- Ref.read available
                      let currentHead = uncons availableNow
                      case currentHead of
                        Nothing -> pure unit
                        Just { head, tail } -> do
                          void $ Ref.write tail available
                          myUnsubId <- ids
                          myUnsub <- Ref.new (pure unit)
                          eltsUnsubId <- ids
                          eltsUnsub <- Ref.new (pure unit)
                          myImmediateCancellation <- Ref.new (pure unit)
                          stageRef <- Ref.new Begin
                          c0 <- runSTFn2 mySub inner $ mkSTFn1 \kid' -> do
                            stage <- Ref.read stageRef
                            case kid', stage of
                              Release, Middle -> do
                                void $ Ref.write End stageRef
                                -- we may need to run the cancellation twice
                                -- because the refs are not populated yet if it runs
                                -- immediately
                                -- thus the `nub` below
                                let
                                  mic =
                                    -- use nub so that the operation is idempotent
                                    -- as we may call it twice
                                    -- if the cancelation is immediate
                                    do
                                      void $ Ref.modify
                                        ((_ <> [ head ]) >>> nub)
                                        available
                                      join (Ref.read myUnsub)
                                      join (Ref.read eltsUnsub)
                                      void $ Ref.modify
                                        (Object.delete myUnsubId)
                                        cancelInner
                                      void $ Ref.modify
                                        (Object.delete eltsUnsubId)
                                        cancelInner

                                do
                                  void $ Ref.write mic myImmediateCancellation
                                  mic
                              Acquire kid, Begin -> do
                                -- holds the current id
                                void $ Ref.write Middle stageRef
                                c1 <- runSTFn2 mySub
                                  ( ( (\(C.Instance i) -> i) $ kid
                                        ( InstanceId
                                            { meshId: me, instanceId: head }
                                        )
                                    ) di
                                  )
                                  k
                                void $ Ref.modify
                                  (Object.insert eltsUnsubId c1)
                                  cancelInner
                                void $ Ref.write c1 eltsUnsub
                              _, _ -> pure unit
                          void $ Ref.write c0 myUnsub
                          void $ Ref.modify (Object.insert myUnsubId c0)
                            cancelInner
                          mycan <- Ref.read myImmediateCancellation
                          mycan
                pure do
                  Ref.read cancelInner >>= traverse_ identity
                  cancelOuter
            ]
      )
      topK
    pure do
      runSTFn1 topK (deleteFromCache { id: me })
      u0
      u1