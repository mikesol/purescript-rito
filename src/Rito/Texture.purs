module Rito.Texture where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, makeAff)
import Effect.Exception (Error)
import Rito.THREE as THREE

data Texture
data TextureLoader

foreign import loader :: THREE.TTextureLoader -> Effect TextureLoader

foreign import load
  :: TextureLoader
  -> String
  -> (Texture -> Effect Unit)
  -> (Error -> Effect Unit)
  -> Effect Unit

loadAff :: TextureLoader -> String -> Aff Texture
loadAff l url = makeAff \f -> do
  load l url (Right >>> f) (Left >>> f)
  mempty