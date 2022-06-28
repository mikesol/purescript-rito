module Rito.Renderers.WebGL.GlitchPass where

import Prelude

import Bolson.Core as Bolson
import Data.Foldable (oneOf)
import FRP.Event (bang, makeEvent, subscribe)
import Rito.Core as C
import Rito.THREE as THREE

-- todo: copy-paste from webgl renderer with the avars. fix?
glitchPass
  :: forall lock payload
   . { glitchPass :: THREE.TGlitchPass }
  -> C.APass lock payload
glitchPass ii = Bolson.Element' $ C.Pass go
  where
  go
    psr
      ( C.ThreeInterpret
          { ids
          , deleteFromCache
          , makeGlitchPass
          }
      ) = makeEvent \k0 -> do
    me <- ids
    psr.raiseId me
    u1 <- subscribe
          ( oneOf
              [ bang $ makeGlitchPass
                  { id: me
                  , parent: psr.parent
                  , glitchPass: ii.glitchPass
                  }
              ]
          )
          k0
    pure (k0 (deleteFromCache { id: me }) *> u1)
