module Rito.InstancedMesh
  ( instancedMesh
  , instancedMesh'
  , setter
  , InstancedMesh(..)
  , InstancedMesh'
  , Setter
  , SetterF
  ) where

import Prelude

import Bolson.Core as Bolson
import Control.Plus (empty)
import Data.Exists (Exists, mkExists, runExists)
import Data.FastVect.Common (class IsVect)
import Data.Foldable (oneOf)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Reflectable (class Reflectable, reflectType)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Variant (Variant, match)
import Effect.Ref as Ref
import FRP.Event (Event, bang, makeEvent, subscribe)
import Rito.Color (Color)
import Rito.Core as C
import Rito.Matrix4 (Matrix4)
import Rito.THREE as THREE
import Type.Proxy (Proxy(..))

----
newtype SetterF x f = SetterF
  { f :: f x
  , t :: forall m. Applicative m => (Int -> x -> m Unit) -> f x -> m Unit
  }
newtype Setter (n :: Int) x = Setter (Exists (SetterF x))
type InstancedMesh' (n :: Int) = Variant
  (setMatrix4 :: Setter n Matrix4, setColor :: Setter n Color)
newtype InstancedMesh (n :: Int) = InstancedMesh (InstancedMesh' n)
instance Newtype (InstancedMesh n) (InstancedMesh' n)

setter :: forall n f x. IsVect (f n) => f n x -> Setter n x
setter f = Setter $ mkExists
  ( SetterF
      { f
      , t: (map <<< map) void traverseWithIndex
      }
  )

instancedMesh
  :: forall n lock payload
   . Reflectable n Int
  => { matrix4 :: THREE.TMatrix4
     , mesh :: THREE.TMesh
     , instancedMesh :: THREE.TInstancedMesh
     }
  -> C.Geometry lock payload
  -> C.Material lock payload
  -> Event (InstancedMesh n)
  -> C.AMesh lock payload
instancedMesh = instancedMesh' Proxy

instancedMesh'
  :: forall n lock payload
   . Reflectable n Int
  => Proxy n
  -> { matrix4 :: THREE.TMatrix4
     , mesh :: THREE.TMesh
     , instancedMesh :: THREE.TInstancedMesh
     }
  -> C.Geometry lock payload
  -> C.Material lock payload
  -> Event (InstancedMesh n)
  -> C.AMesh lock payload
instancedMesh' _ imsh (C.Geometry geo) (C.Material mat) props = Bolson.Element'
  $
    C.Mesh
      go
  where
  go
    parent
    di@
      ( C.ThreeInterpret
          { ids
          , deleteFromCache
          , makeInstancedMesh
          , setInstancedMeshMatrix4
          , setInstancedMeshColor
          }
      ) = makeEvent \k -> do
    me <- ids
    geoR <- Ref.new Nothing
    matR <- Ref.new Nothing
    parent.raiseId me
    u0 <- flip subscribe k $
      oneOf
        [ geo
            -- we set the parent to nothing
            -- because we need to attribute the parent
            -- in makeInstancedMesh
            { parent: Nothing
            , scope: parent.scope
            , raiseId: \id -> Ref.write (Just id) geoR
            }
            di
        , mat
            { parent: Nothing
            , scope: parent.scope
            , raiseId: \id -> Ref.write (Just id) matR
            }
            di
        ]
    geoId <- Ref.read geoR
    matId <- Ref.read matR
    u1 <- flip subscribe k $ case geoId, matId of
      Nothing, _ -> empty
      _, Nothing -> empty
      Just gid, Just mid -> oneOf
        [ bang $ makeInstancedMesh
            { id: me
            , parent: parent.parent
            , scope: parent.scope
            , geometry: gid
            , material: mid
            , instancedMesh: imsh.instancedMesh
            , matrix4: imsh.matrix4
            , mesh: imsh.mesh
            , count: reflectType (Proxy :: _ n)
            }
        , props <#>
            ( \(InstancedMesh msh) ->
                msh # match
                  { setMatrix4: \(Setter v) -> setInstancedMeshMatrix4 $
                      { id: me
                      , setMatrix4: \f -> v # runExists \(SetterF i) -> i.t f
                          i.f
                      }
                  , setColor: \(Setter v) -> setInstancedMeshColor $
                      { id: me
                      , setColor: \f -> v # runExists \(SetterF i) -> i.t f i.f
                      }
                  }
            )
        ]
    pure (k (deleteFromCache { id: me }) *> u0 *> u1)