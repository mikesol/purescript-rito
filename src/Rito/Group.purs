module Rito.Group (group, unsafeInternalGroup, Group(..)) where

import Prelude

import Bolson.Control (flatten)
import Bolson.Core (Entity(..), Scope, fixed)
import Bolson.Core as Bolson
import Control.Monad.ST.Uncurried (mkSTFn2, runSTFn1, runSTFn2)
import Data.Foldable (oneOf)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Variant (Variant, match)
import FRP.Event (Event, Subscriber(..), makeLemmingEventO)
import Rito.Core (ThreeInterpret(..))
import Rito.Core as C
import Rito.THREE as THREE
import Unsafe.Coerce (unsafeCoerce)

newtype Group = Group
  (Variant (| C.Object3D))

derive instance Newtype Group _

group
  :: forall payload
   . { group :: THREE.TGroup }
  -> Event Group
  -> Array (C.AGroupful payload)
  -> C.AGroup payload
group = unsafeInternalGroup \(ThreeInterpret { makeGroup }) -> makeGroup

unsafeInternalGroup
  :: forall group payload
   . ( ThreeInterpret payload
       -> { id :: String
          , scope :: Scope
          , parent :: Maybe String
          , group :: group
          }
       -> payload
     )
  -> { group :: group }
  -> Event Group
  -> Array (C.AGroupful payload)
  -> C.AGroup payload
unsafeInternalGroup dif gp props kidz = Element' $ C.Group go
  where
  go
    parent
    di@
      ( C.ThreeInterpret
          { ids
          , deleteFromCache
          }
      ) = makeLemmingEventO $ mkSTFn2 \(Subscriber mySub) k -> do
    me <- ids
    parent.raiseId me
    unsub <- runSTFn2 mySub
      ( oneOf
          [ pure $ dif di
              { id: me
              , parent: parent.parent
              , scope: parent.scope
              , group: gp.group
              }
          , props <#>
              ( \(Group msh) ->
                  msh # match (C.object3D me di)
              )
          , flatten
              { doLogic: absurd
              , ids: unwrap >>> _.ids
              , disconnectElement: unwrap >>> _.disconnect
              , toElt: \(C.Group obj) -> Bolson.Element obj
              }
              { parent: Just me, scope: parent.scope, raiseId: \_ -> pure unit }
              di
              ( fixed
                  ( map
                      ( unsafeCoerce
                          :: C.AGroupful payload
                          -> C.AGroup payload
                      )
                      kidz
                  )
              )
          ]
      )
      k
    pure do
      runSTFn1 k (deleteFromCache { id: me })
      unsub
