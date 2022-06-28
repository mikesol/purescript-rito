module Rito.Renderers.WebGL.BloomPass where

import Prelude

import Bolson.Core as Bolson
import Data.Foldable (oneOf)
import FRP.Event (bang, makeEvent, subscribe)
import Rito.Core as C
import Rito.THREE as THREE

-- todo: copy-paste from webgl renderer with the avars. fix?
bloomPass
  :: forall lock payload
   . { bloomPass :: THREE.TBloomPass }
  -> C.APass lock payload
bloomPass ii = Bolson.Element' $ C.Pass go
  where
  go
    psr
      ( C.ThreeInterpret
          { ids
          , deleteFromCache
          , makeBloomPass
          }
      ) = makeEvent \k0 -> do
    me <- ids
    psr.raiseId me
    u1 <- subscribe
          ( oneOf
              [ bang $ makeBloomPass
                  { id: me
                  , parent: psr.parent
                  , bloomPass: ii.bloomPass
                  }
              ]
          )
          k0
    pure (k0 (deleteFromCache { id: me }) *> u1)
