module Rito.Uniforms where

import Foreign (Foreign)
import Prim.RowList as RL
import Rito.Texture (Texture)
import Rito.Vector2 (Vector2)
import Rito.Vector3 (Vector3)

class IsUniform (a :: RL.RowList Type)

instance IsUniform RL.Nil
instance IsUniform rest => IsUniform (RL.Cons key Texture rest)
instance IsUniform rest => IsUniform (RL.Cons key Vector3 rest)
instance IsUniform rest => IsUniform (RL.Cons key Vector2 rest)
instance IsUniform rest => IsUniform (RL.Cons key Number rest)

foreign import toUniformDeclImpl :: forall a. { | a } -> Foreign

toUniformDecl :: forall a arl. RL.RowToList a arl => IsUniform arl => { | a } -> Foreign
toUniformDecl = toUniformDeclImpl