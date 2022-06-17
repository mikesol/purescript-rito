module Rito.Materials.RawShaderMaterial
  ( rawShaderMaterial
  , rawShaderMaterial_
  , RawShaderMaterial(..)
  , RawShaderMaterial'
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Newtype (class Newtype)
import Data.Symbol (reflectSymbol)
import Data.Variant (Unvariant(..), Variant, match, unvariant)
import FRP.Event (Event, bang, makeEvent, subscribe)
import Foreign (Foreign)
import Prim.RowList (class RowToList)
import Rito.Core as C
import Rito.THREE as THREE
import Rito.Uniforms (class IsUniform, toUniformDecl)
import Unsafe.Coerce (unsafeCoerce)

type RawShaderMaterial' u = Variant
  ( uniform :: Variant u
  )

newtype RawShaderMaterial u = RawShaderMaterial (RawShaderMaterial' u)
instance Newtype (RawShaderMaterial u) (RawShaderMaterial' u)

rawShaderMaterial
  :: forall u url lock payload
   . RowToList u url
  => IsUniform url
  => { rawShaderMaterial :: THREE.TRawShaderMaterial
     , uniforms :: { | u }
     , fragmentShader :: String
     , vertexShader :: String
     }
  -> Event (RawShaderMaterial u)
  -> C.Material lock payload
rawShaderMaterial i atts = C.Material go
  where
  go
    parent
    ( C.ThreeInterpret
        { ids
        , deleteFromCache
        , makeRawShaderMaterial
        , setUniform
        }
    ) = makeEvent \k -> do
    me <- ids
    parent.raiseId me
    map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
      bang
        ( makeRawShaderMaterial
            { id: me
            , parent: parent.parent
            , scope: parent.scope
            , rawShaderMaterial: i.rawShaderMaterial
            , uniforms: toUniformDecl i.uniforms
            , fragmentShader: i.fragmentShader
            , vertexShader: i.vertexShader
            }
        )
        <|>
          ( map
              ( \(RawShaderMaterial e) -> match
                  { uniform: \v -> do
                      let Unvariant uv = unvariant v
                      let
                        { key, value } = uv \k v ->
                          { key: reflectSymbol k
                          , value: ((unsafeCoerce :: forall a. a -> Foreign) v)
                          }
                      setUniform { id: me, key, value }
                  }
                  e
              )
              atts
          )

rawShaderMaterial_
  :: forall u url lock payload
   . RowToList u url
  => IsUniform url
  => { rawShaderMaterial :: THREE.TRawShaderMaterial
     , uniforms :: { | u }
     , fragmentShader :: String
     , vertexShader :: String
     }
  -> C.Material lock payload
rawShaderMaterial_ i = rawShaderMaterial i empty
