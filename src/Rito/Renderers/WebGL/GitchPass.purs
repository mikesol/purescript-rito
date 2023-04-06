module Rito.Renderers.WebGL.GlitchPass where

import Prelude

import Bolson.Core as Bolson
import Control.Monad.ST.Uncurried (mkSTFn2, runSTFn1, runSTFn2)
import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Data.Foldable (oneOf)
import FRP.Event (Subscriber(..), makeLemmingEventO)
import Rito.Core as C
import Rito.THREE as THREE

data GlitchPassOptions = GlitchPassOptions

instance
  ConvertOption GlitchPassOptions
    "glitchPass"
    THREE.TGlitchPass
    THREE.TGlitchPass where
  convertOption _ _ = identity

instance
  ConvertOption GlitchPassOptions
    "dtSize"
    Int
    Int where
  convertOption _ _ = identity


type GlitchPassOptional =
  ( dtSize :: Int
  )

type GlitchPassAll =
  (glitchPass :: THREE.TGlitchPass | GlitchPassOptional)

defaultGlitchPass :: { | GlitchPassOptional }
defaultGlitchPass =
  { dtSize: 64
  }

class InitialGlitchPass i where
  toInitializeGlitchPass :: i -> C.InitializeGlitchPass

instance InitialGlitchPass C.InitializeGlitchPass where
  toInitializeGlitchPass = identity

instance
  ConvertOptionsWithDefaults GlitchPassOptions { | GlitchPassOptional } { | provided }
    { | GlitchPassAll } =>
  InitialGlitchPass { | provided } where
  toInitializeGlitchPass provided = C.InitializeGlitchPass
    (convertOptionsWithDefaults GlitchPassOptions defaultGlitchPass provided)

glitchPass
  :: forall i payload
   . InitialGlitchPass i
  => i
  -> C.APass payload
glitchPass ii' = Bolson.Element' $ C.Pass go
  where
  C.InitializeGlitchPass ii = toInitializeGlitchPass ii'
  go
    psr
      ( C.ThreeInterpret
          { ids
          , deleteFromCache
          , makeGlitchPass
          }
      ) = makeLemmingEventO $ mkSTFn2 \(Subscriber mySub) k0 -> do
    me <- ids
    psr.raiseId me
    u1 <- runSTFn2 mySub
          ( oneOf
              [ pure $ makeGlitchPass
                  { id: me
                  , parent: psr.parent
                  , glitchPass: ii.glitchPass
                  , dtSize: ii.dtSize
                  }
              ]
          )
          k0
    pure do
      runSTFn1 k0 (deleteFromCache { id: me })
      u1
