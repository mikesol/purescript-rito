module Rito.Renderers.WebGL.BloomPass where

import Prelude

import Bolson.Core as Bolson
import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Data.Foldable (oneOf)
import FRP.Event (bang, makeEvent, subscribe)
import Rito.Core as C
import Rito.THREE as THREE

data BloomPassOptions = BloomPassOptions

instance
  ConvertOption BloomPassOptions
    "bloomPass"
    THREE.TBloomPass
    THREE.TBloomPass where
  convertOption _ _ = identity

instance
  ConvertOption BloomPassOptions
    "strength"
    Number
    Number where
  convertOption _ _ = identity

instance
  ConvertOption BloomPassOptions
    "kernelSize"
    Int
    Int where
  convertOption _ _ = identity

instance
  ConvertOption BloomPassOptions
    "sigma"
    Number
    Number where
  convertOption _ _ = identity

instance
  ConvertOption BloomPassOptions
    "resolution"
    Int
    Int where
  convertOption _ _ = identity

type BloomPassOptional =
  ( strength :: Number
  , kernelSize :: Int
  , sigma :: Number
  , resolution :: Int

  )

type BloomPassAll =
  (bloomPass :: THREE.TBloomPass | BloomPassOptional)

defaultBloomPass :: { | BloomPassOptional }
defaultBloomPass =
  { strength: 1.0
  , kernelSize: 25
  , sigma: 4.0
  , resolution: 256
  }

class InitialBloomPass i where
  toInitializeBloomPass :: i -> C.InitializeBloomPass

instance InitialBloomPass C.InitializeBloomPass where
  toInitializeBloomPass = identity

instance
  ConvertOptionsWithDefaults BloomPassOptions { | BloomPassOptional }
    { | provided }
    { | BloomPassAll } =>
  InitialBloomPass { | provided } where
  toInitializeBloomPass provided = C.InitializeBloomPass
    (convertOptionsWithDefaults BloomPassOptions defaultBloomPass provided)

bloomPass
  :: forall i lock payload
   . InitialBloomPass i
  => i
  -> C.APass lock payload
bloomPass ii' = Bolson.Element' $ C.Pass go
  where
  C.InitializeBloomPass ii = toInitializeBloomPass ii'
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
              , strength: ii.strength
              , kernelSize: ii.kernelSize
              , sigma: ii.sigma
              , resolution: ii.resolution
              }
          ]
      )
      k0
    pure (k0 (deleteFromCache { id: me }) *> u1)
