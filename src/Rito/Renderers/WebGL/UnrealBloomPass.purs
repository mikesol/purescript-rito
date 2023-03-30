module Rito.Renderers.WebGL.UnrealBloomPass where

import Prelude

import Bolson.Core as Bolson
import Control.Monad.ST.Uncurried (mkSTFn2, runSTFn1, runSTFn2)
import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Data.Foldable (oneOf)
import Data.Variant (Variant, match)
import FRP.Event (Event, Subscriber(..), makeLemmingEventO)
import Rito.Core as C
import Rito.THREE as THREE
import Rito.Vector2 (Vector2)

data UnrealBloomPassOptions = UnrealBloomPassOptions

instance
  ConvertOption UnrealBloomPassOptions
    "unrealBloomPass"
    THREE.TUnrealBloomPass
    THREE.TUnrealBloomPass where
  convertOption _ _ = identity

instance
  ConvertOption UnrealBloomPassOptions
    "resolution"
    Vector2
    Vector2 where
  convertOption _ _ = identity

instance
  ConvertOption UnrealBloomPassOptions
    "strength"
    Number
    Number where
  convertOption _ _ = identity

instance
  ConvertOption UnrealBloomPassOptions
    "radius"
    Number
    Number where
  convertOption _ _ = identity

instance
  ConvertOption UnrealBloomPassOptions
    "threshold"
    Number
    Number where
  convertOption _ _ = identity

type UnrealBloomPassOptional :: forall k. Row k
type UnrealBloomPassOptional =
  (
  )

type UnrealBloomPassAll =
  ( unrealBloomPass :: THREE.TUnrealBloomPass
  , resolution :: Vector2
  , strength :: Number
  , radius :: Number
  , threshold :: Number
  | UnrealBloomPassOptional
  )

defaultUnrealBloomPass :: { | UnrealBloomPassOptional }
defaultUnrealBloomPass = {}

class InitialUnrealBloomPass i where
  toInitializeUnrealBloomPass :: i -> C.InitializeUnrealBloomPass

instance InitialUnrealBloomPass C.InitializeUnrealBloomPass where
  toInitializeUnrealBloomPass = identity

instance
  ConvertOptionsWithDefaults UnrealBloomPassOptions
    { | UnrealBloomPassOptional }
    { | provided }
    { | UnrealBloomPassAll } =>
  InitialUnrealBloomPass { | provided } where
  toInitializeUnrealBloomPass provided = C.InitializeUnrealBloomPass
    ( convertOptionsWithDefaults UnrealBloomPassOptions defaultUnrealBloomPass
        provided
    )

newtype UnrealBloomPass = UnrealBloomPass
  ( Variant
      ( resolution :: Vector2
      , strength :: Number
      , radius :: Number
      , threshold :: Number
      )
  )

unrealBloomPass
  :: forall i payload
   . InitialUnrealBloomPass i
  => i
  -> Event UnrealBloomPass
  -> C.APass payload
unrealBloomPass ii' propz = Bolson.Element' $ C.Pass go
  where
  C.InitializeUnrealBloomPass ii = toInitializeUnrealBloomPass ii'
  go
    psr
    ( C.ThreeInterpret
        { ids
        , deleteFromCache
        , makeUnrealBloomPass
        , setResolution
        , setStrength
        , setRadius
        , setThreshold
        }
    ) = makeLemmingEventO $ mkSTFn2 \(Subscriber mySub) k0 -> do
    me <- ids
    psr.raiseId me
    u1 <- runSTFn2 mySub
      ( oneOf
          [ pure $ makeUnrealBloomPass
              { id: me
              , parent: psr.parent
              , unrealBloomPass: ii.unrealBloomPass
              , resolution: ii.resolution
              , strength: ii.strength
              , radius: ii.radius
              , threshold: ii.threshold
              }
          , map
              ( \(UnrealBloomPass e) -> match
                  { resolution: setResolution <<< { id: me, resolution: _ }
                  , strength: setStrength <<< { id: me, strength: _ }
                  , radius: setRadius <<< { id: me, radius: _ }
                  , threshold: setThreshold <<< { id: me, threshold: _ }
                  }
                  e
              )
              propz
          ]
      )
      k0
    pure do
      runSTFn1 k0 (deleteFromCache { id: me })
      u1
