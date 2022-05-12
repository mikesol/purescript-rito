module Rito.Scene (scene, Scene) where

import Prelude

import Bolson.Control (flatten)
import Bolson.Core (Entity(..), fixed)
import Bolson.Core as Bolson
import Control.Plus (empty)
import Data.Foldable (oneOf)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Variant (Variant, match)
import FRP.Event (Event, bang, makeEvent, subscribe)
import Rito.Core as C
import Rito.Group as Group
import Safe.Coerce (coerce)
import Unsafe.Coerce (unsafeCoerce)

newtype Scene = Scene
  (Variant (| C.Object3D))

scene
  :: forall lock payload
   . Event Scene
  -> Array (C.ASceneful lock payload)
  -> C.AScene lock payload
scene props kidz = Element' $ C.Scene go
  where
  go
    parent
    di@
      ( C.ThreeInterpret
          { ids
          , deleteFromCache
          , makeScene
          }
      ) = makeEvent \k -> do
    me <- ids
    parent.raiseId me
    map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
      oneOf
        [ bang $ makeScene
            { id: me
            , parent: parent.parent
            , scope: parent.scope
            }
        , props <#>
            ( \(Scene msh) ->
                msh # match (C.object3D me di)
            )
        , flatten
            { doLogic: absurd
            , ids: unwrap >>> _.ids
            , disconnectElement: unwrap >>> _.disconnect
            , wrapElt: \a ->
                (unsafeCoerce :: C.AGroup lock payload -> C.AScene lock payload)
                  (Group.group empty [ coerce a ])
            , toElt: \(C.Scene obj) -> Bolson.Element obj
            }
            { parent: Just me, scope: parent.scope, raiseId: pure mempty }
            di
            ( fixed
                ( map
                    ( unsafeCoerce
                        :: C.ASceneful lock payload
                        -> C.AScene lock payload
                    )
                    kidz
                )
            )
        ]
