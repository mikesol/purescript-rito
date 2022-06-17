module Rito.Materials.ShaderMaterial
  ( shaderMaterial
  , shaderMaterial_
  , ShaderMaterial(..)
  , ShaderMaterial'
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

type ShaderMaterial' u = Variant
  ( uniform :: Variant u
  )

newtype ShaderMaterial u = ShaderMaterial (ShaderMaterial' u)
instance Newtype (ShaderMaterial u) (ShaderMaterial' u)

shaderMaterial
  :: forall u url lock payload
   . RowToList u url
  => IsUniform url
  => { shaderMaterial :: THREE.TShaderMaterial
     , uniforms :: { | u }
     , fragmentShader :: String
     , vertexShader :: String
     }
  -> Event (ShaderMaterial u)
  -> C.Material lock payload
shaderMaterial i atts = C.Material go
  where
  go
    parent
    ( C.ThreeInterpret
        { ids
        , deleteFromCache
        , makeShaderMaterial
        , setUniform
        }
    ) = makeEvent \k -> do
    me <- ids
    parent.raiseId me
    map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
      bang
        ( makeShaderMaterial
            { id: me
            , parent: parent.parent
            , scope: parent.scope
            , shaderMaterial: i.shaderMaterial
            , uniforms: toUniformDecl i.uniforms
            , fragmentShader: i.fragmentShader
            , vertexShader: i.vertexShader
            }
        )
        <|>
          ( map
              ( \(ShaderMaterial e) -> match
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

shaderMaterial_
  :: forall u url lock payload
   . RowToList u url
  => IsUniform url
  => { shaderMaterial :: THREE.TShaderMaterial
     , uniforms :: { | u }
     , fragmentShader :: String
     , vertexShader :: String
     }
  -> C.Material lock payload
shaderMaterial_ i = shaderMaterial i empty
