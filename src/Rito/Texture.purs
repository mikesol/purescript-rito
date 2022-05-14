module Rito.Texture where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, makeAff)
import Effect.Exception (Error)

data Texture

foreign import load
  :: String -> (Texture -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit

loadAff :: String -> Aff Texture
loadAff url = makeAff \f -> load url (Right >>> f) (Left >>> f) *> mempty