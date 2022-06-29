module Rito.Renderers.WebGL.UnrealBloomPass where

import Prelude

import Bolson.Core as Bolson
import Data.Foldable (oneOf)
import FRP.Event (bang, makeEvent, subscribe)
import Rito.Core as C
import Rito.THREE as THREE

-- todo: copy-paste from webgl renderer with the avars. fix?
unrealBloomPass
  :: forall lock payload
   . { unrealBloomPass :: THREE.TUnrealBloomPass }
  -> C.APass lock payload
unrealBloomPass ii = Bolson.Element' $ C.Pass go
  where
  go
    psr
      ( C.ThreeInterpret
          { ids
          , deleteFromCache
          , makeUnrealBloomPass
          }
      ) = makeEvent \k0 -> do
    me <- ids
    psr.raiseId me
    u1 <- subscribe
          ( oneOf
              [ bang $ makeUnrealBloomPass
                  { id: me
                  , parent: psr.parent
                  , unrealBloomPass: ii.unrealBloomPass
                  }
              ]
          )
          k0
    pure (k0 (deleteFromCache { id: me }) *> u1)
