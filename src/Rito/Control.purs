module Rito.Control
  ( blank
  , globalPortal
  , portal
  , switcher
  , class Connectable
  , toCtor
  , fromCtor
  , connect
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Filterable (filter)
import Data.Foldable (oneOf)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Profunctor (lcmap)
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Vec (toArray, Vec)
import Effect (Effect, foreachE)
import Effect.AVar (tryPut)
import Effect.AVar as AVar
import Effect.Exception (throwException)
import FRP.Event (Event, bang, keepLatest, makeEvent, mapAccum, memoize, subscribe)
import Rito.Core (Child(..), Ctor, DynamicChildren(..), Mesh(..), ThreeInterpret(..), Threeful(..))
import Safe.Coerce (coerce)
import Type.Equality (proof)
import Unsafe.Coerce (unsafeCoerce)

newtype MutAr a = MutAr (Array a)

foreign import mutAr :: forall a. Array a -> Effect (MutAr a)
foreign import unsafeUpdateMutAr :: forall a. Int -> a -> MutAr a -> Effect Unit
foreign import readAr :: forall a. MutAr a -> Effect (Array a)

class Connectable :: forall k. (k -> Type -> Type) -> Constraint
class Connectable ctor where
  toCtor :: forall lock payload. ctor lock payload -> Ctor payload
  fromCtor :: forall lock payload. Ctor payload -> ctor lock payload
  connect :: forall lock payload. String -> ctor lock payload

internalPortal
  :: forall n ctor final lock0 lock1 payload
   . Connectable ctor
  => Connectable final
  => Boolean
  -> (String -> String)
  -> Vec n (ctor lock0 payload)
  -> ( Vec n (ctor lock1 payload)
       -> (ctor lock0 payload -> ctor lock1 payload)
       -> final lock1 payload
     )
  -> final lock0 payload
internalPortal isGlobal scopeF gaga closure = fromCtor go
  where
  go psr di@(ThreeInterpret { deleteFromCache }) = makeEvent \k -> do
    av <- mutAr (map (const "") $ toArray gaga)
    let
      actualized = oneOf $ mapWithIndex
        ( \ix -> lcmap toCtor \gogo ->
            gogo
              { parent: "@portal@"
              , scope: scopeF psr.scope
              , raiseId: \id -> unsafeUpdateMutAr ix id av
              }
              di
        )
        gaga
    u0 <- subscribe actualized k
    av2 <- AVar.empty
    let
      asIds :: Array String -> Vec n String
      asIds = unsafeCoerce
    idz <- asIds <$> readAr av
    let
      -- we never connect or disconnect the referentially opaque node
      -- instead, it is always managed inside a referentially transparent node
      -- that can be properly connected and disconnected
      injectable = map connect idz
      realized = toCtor (closure injectable (\x -> fromCtor (toCtor x))) psr di
    u <- subscribe realized k
    void $ tryPut u av2
    -- cancel immediately, as it should be run synchronously
    -- so if this actually does something then we have a problem
    pure do
      u0
      when (not isGlobal) $ foreachE (toArray idz) \id -> k
        (deleteFromCache { id })
      cncl2 <- AVar.take av2 \q -> case q of
        Right usu -> usu
        Left e -> throwException e
      -- cancel immediately, as it should be run synchronously
      -- so if this actually does something then we have a problem
      cncl2

globalPortal
  :: forall n ctor final lock payload
   . Connectable ctor
  => Connectable final
  => Vec n (ctor lock payload)
  -> (Vec n (ctor lock payload) -> final lock payload)
  -> final lock payload
globalPortal e f = internalPortal true (const "@portal@") e (\x _ -> f x)

portal
  :: forall n ctor final lock0 payload
   . Connectable ctor
  => Connectable final
  => Vec n (ctor lock0 payload)
  -> ( forall lock1
        . Vec n (ctor lock1 payload)
       -> (ctor lock0 payload -> ctor lock1 payload)
       -> final lock1 payload
     )
  -> final lock0 payload
portal e = internalPortal false identity e

blank :: forall lock payload. Mesh lock payload
blank = Mesh go
  where
  go
    { parent, scope, raiseId }
    (ThreeInterpret { ids, makeNoop, deleteFromCache }) = makeEvent
    \k -> do
      me <- ids
      raiseId me
      map ((*>) (k (deleteFromCache { id: me }))) $ subscribe
        (bang (makeNoop { id: me, parent, scope }))
        k

switcher
  :: forall i lock payload
   . (i -> Mesh lock payload)
  -> Event i
  -> Threeful lock payload
switcher f event = DynamicChildren'
  (DynamicChildren (proof (coerce i)))
  where
  i = keepLatest
    $ memoize (counter event) \cenv -> map
      ( \(p /\ n) -> bang (Add $ f p) <|>
          ((const Remove) <$> filter (eq (n + 1) <<< snd) cenv)
      )
      cenv
  counter :: forall a. Event a → Event (a /\ Int)
  counter ev = mapAccum fn ev 0
    where
    fn a b = (b + 1) /\ (a /\ b)