module Rito.Group (group, Group(..)) where

import Prelude

import Bolson.Control (flatten)
import Bolson.Core (Entity(..), fixed)
import Bolson.Core as Bolson
import Control.Plus (empty)
import Data.Foldable (oneOf)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Variant (Variant, match)
import FRP.Event (Event, bang, makeEvent, subscribe)
import Rito.Core as C
import Unsafe.Coerce (unsafeCoerce)

newtype Group = Group
  (Variant (| C.Object3D))

derive instance Newtype Group _

group
  :: forall lock payload
   . Event Group
  -> Array (C.AGroupful lock payload)
  -> C.AGroup lock payload
group props kidz = Element' $ C.Group go
  where
  go
    parent
    di@
      ( C.ThreeInterpret
          { ids
          , deleteFromCache
          , makeGroup
          }
      ) = makeEvent \k -> do
    me <- ids
    parent.raiseId me
    map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
      oneOf
        [ bang $ makeGroup
            { id: me
            , parent: parent.parent
            , scope: parent.scope
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
            { parent: Just me, scope: parent.scope, raiseId: pure mempty }
            di
            ( fixed
                ( map
                    ( unsafeCoerce
                        :: C.AGroupful lock payload
                        -> C.AGroup lock payload
                    )
                    kidz
                )
            )
        ]
