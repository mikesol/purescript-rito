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
import FRP.Event (Event, bang, makeEvent, subscribe)
import Rito.Core (toGroup)
import Rito.Core as C
import Rito.Group as Group
import Unsafe.Coerce (unsafeCoerce)

----
type Mesh' = Variant (| C.Object3D)
newtype Mesh = Mesh Mesh'
instance Newtype Mesh Mesh'

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
          }
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
        , props <#>
            ( \(Mesh msh) ->
                msh # match (C.object3D me di)
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