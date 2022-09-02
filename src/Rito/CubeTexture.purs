module Rito.CubeTexture where

import Prelude

import Data.Either (Either(..))
import Data.FastVect.FastVect (Vect, empty, (:))
import Effect (Effect)
import Effect.Aff (Aff, makeAff)
import Effect.Exception (Error)
import Rito.THREE as THREE

data CubeTexture
data CubeTextureLoader

foreign import loader :: THREE.TCubeTextureLoader -> Effect CubeTextureLoader

foreign import load
  :: CubeTextureLoader
  -> Vect 6 String
  -> (CubeTexture -> Effect Unit)
  -> (Error -> Effect Unit)
  -> Effect Unit

loadAff :: CubeTextureLoader -> Vect 6 String -> Aff CubeTexture
loadAff l url = makeAff \f -> do
  load l url (Right >>> f) (Left >>> f)
  mempty

loadAffRecord :: CubeTextureLoader -> {right::String,left::String,top::String,bottom::String,front::String,back::String} -> Aff CubeTexture
loadAffRecord l u = loadAff l (u.right : u.left : u.top : u.bottom : u.front : u.back : empty)