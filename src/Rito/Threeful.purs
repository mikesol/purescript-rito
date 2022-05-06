module Rito.Threeful (Threeful(..), Child(..), DynamicChildren(..), FixedChildren(..), Eventful(..), handleKids) where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (fold, oneOfMap, traverse_)
import Data.Maybe (Maybe(..))
import Effect.AVar (tryPut)
import Effect.AVar as AVar
import Effect.Exception (throwException)
import Effect.Ref as Ref
import FRP.Event (Event, keepLatest, makeEvent, subscribe)
import Foreign.Object as Object
import Rito.Core as C

data Child :: forall k1 k2. (k1 -> k2 -> Type) -> k1 -> k2 -> Type
data Child obj lock payload = Add (obj lock payload) | Remove

newtype DynamicChildren :: forall k1 k2. (k1 -> k2 -> Type) -> k1 -> k2 -> Type
newtype DynamicChildren obj lock payload = DynamicChildren
  (Event (Event (Child obj lock payload)))

newtype FixedChildren obj lock payload = FixedChildren
  (Array (Threeful obj lock payload))

newtype Eventful obj lock payload = Eventful
  (Event (Threeful obj lock payload))

data Threeful obj lock payload
  = DynamicChildren' (DynamicChildren obj lock payload)
  | FixedChildren' (FixedChildren obj lock payload)
  | Eventful' (Eventful obj lock payload)
  | PlainOld' (obj lock payload)

data Stage = Begin | Middle | End

handleKids
  :: forall obj lock payload
   . C.Connectable obj
  => String
  -> String
  -> C.ThreeInterpret payload
  -> Threeful obj lock payload
  -> Event payload
handleKids
  parent
  scope
  di@(C.ThreeInterpret { ids, disconnect }) = case _ of
  FixedChildren' (FixedChildren f) -> oneOfMap
    (handleKids parent scope di)
    f
  Eventful' (Eventful e) -> keepLatest
    (map (handleKids parent scope di) e)
  PlainOld' e -> element e
  DynamicChildren' (DynamicChildren children) ->
    makeEvent \k -> do
      cancelInner <- Ref.new Object.empty
      cancelOuter <-
        -- each child gets its own scope
        subscribe children \inner ->
          do
            -- holds the previous id
            myUnsubId <- ids
            myUnsub <- Ref.new (pure unit)
            eltsUnsubId <- ids
            eltsUnsub <- Ref.new (pure unit)
            myId <- Ref.new Nothing
            myImmediateCancellation <- Ref.new (pure unit)
            myScope <- ids
            stageRef <- Ref.new Begin
            c0 <- subscribe inner \kid' -> do
              stage <- Ref.read stageRef
              case kid', stage of
                Remove, Middle -> do
                  Ref.write End stageRef
                  let
                    mic =
                      ( Ref.read myId >>= traverse_ \old ->
                          k
                            ( disconnect
                                { id: old, parent, scope: myScope }
                            )
                      ) *> join (Ref.read myUnsub)
                        *> join (Ref.read eltsUnsub)
                        *> Ref.modify_
                          (Object.delete myUnsubId)
                          cancelInner
                        *> Ref.modify_
                          (Object.delete eltsUnsubId)
                          cancelInner
                  Ref.write mic myImmediateCancellation *> mic
                Add kid'', Begin -> do
                  let kid = C.toCtor kid''
                  -- holds the current id
                  Ref.write Middle stageRef
                  av <- AVar.empty
                  c1 <- subscribe
                    ( kid
                        { parent
                        , scope: myScope
                        , raiseId: \id -> do
                            void $ tryPut id av
                        }
                        di
                    )
                    k
                  cncl <- AVar.take av \q -> case q of
                    Right r -> do
                      Ref.write (Just r) (myId)
                      Ref.modify_ (Object.insert eltsUnsubId c1) cancelInner
                      Ref.write c1 eltsUnsub
                    Left e -> throwException e
                  -- cancel immediately, as it should be run synchronously
                  -- so if this actually does something then we have a problem
                  cncl
                -- ignore
                _,
                _ -> pure unit
            Ref.write c0 myUnsub
            Ref.modify_ (Object.insert myUnsubId c0) cancelInner
            join (Ref.read myImmediateCancellation)
      pure do
        Ref.read cancelInner >>= fold
        cancelOuter
  where
  element = C.toCtor >>>
    (\f -> f
        { parent
        , scope
        , raiseId: mempty
        }
          di
    )
