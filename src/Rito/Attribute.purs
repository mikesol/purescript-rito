module Rito.Attribute
  ( Attribute
  , class Attr
  , attr
  , (:=)
  , unsafeUnAttribute
  , unsafeAttribute
  ) where

import Prelude

import Data.Newtype (class Newtype)
import Effect (Effect)
import Safe.Coerce (coerce)
import Web.Event.Internal.Types (Event)

newtype Attribute (e :: Type) v = Attribute
  { key :: String
  , value :: v
  }
unsafeUnAttribute
  :: forall e v. Attribute e v -> { key :: String, value :: v }
unsafeUnAttribute = coerce

unsafeAttribute
  :: forall e v. { key :: String, value :: v } -> Attribute e v
unsafeAttribute = Attribute

class Attr k v e where
  attr :: k -> v -> Attribute e v

infixr 5 attr as :=