module Rito.Geometries.Capsule
  ( capsule
  , capsule_
  , Capsule(..)
  , Capsule'
  , class InitialCapsule
  , CapsuleOptions
  , toInitializeCapsule
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (empty)
import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Data.Newtype (class Newtype)
import Data.Variant (Variant, match)
import FRP.Event (Event, bang, makeEvent, subscribe)
import Record (union)
import Rito.Core as C

data CapsuleOptions = CapsuleOptions

instance
  ConvertOption CapsuleOptions
    "radius"
    Number
    Number where
  convertOption _ _ = identity

instance
  ConvertOption CapsuleOptions
    "length"
    Number
    Number where
  convertOption _ _ = identity

instance
  ConvertOption CapsuleOptions
    "radialSegments"
    Int
    Int where
  convertOption _ _ = identity

instance
  ConvertOption CapsuleOptions
    "capSegments"
    Int
    Int where
  convertOption _ _ = identity

type CapsuleOptional =
  ( radius :: Number
  , length :: Number
  , radialSegments :: Int
  , capSegments :: Int
  )

type CapsuleAll =
  (| CapsuleOptional)

defaultCapsule :: { | CapsuleOptional }
defaultCapsule =
  { radius: 1.0
  , length: 1.0
  , radialSegments: 4
  , capSegments: 8
  }

class InitialCapsule i where
  toInitializeCapsule :: i -> C.InitializeCapsule

instance InitialCapsule C.InitializeCapsule where
  toInitializeCapsule = identity

instance
  ConvertOptionsWithDefaults CapsuleOptions { | CapsuleOptional } { | provided }
    { | CapsuleAll } =>
  InitialCapsule { | provided } where
  toInitializeCapsule provided = C.InitializeCapsule
    (convertOptionsWithDefaults CapsuleOptions defaultCapsule provided)

type Capsule' = Variant
  ( radius :: Number
  , length :: Number
  , radialSegments :: Int
  , capSegments :: Int
  | C.BufferGeometry
  )
newtype Capsule = Capsule Capsule'
instance Newtype Capsule Capsule'

capsule
  :: forall i lock payload
   . InitialCapsule i
  => i
  -> Event Capsule
  -> C.Geometry lock payload
capsule i' atts = C.Geometry go
  where
  C.InitializeCapsule i = toInitializeCapsule i'
  go
    parent
    di@
      ( C.ThreeInterpret
          { ids
          , deleteFromCache
          , makeCapsule
          , setRadius
          , setLength
          , setRadialSegments
          , setCapSegments
          }
      ) = makeEvent \k -> do
    me <- ids
    parent.raiseId me
    map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
      bang
        ( makeCapsule
            { id: me
            , parent: parent.parent
            , scope: parent.scope
            , radius: i.radius
            , length: i.length
            , radialSegments: i.radialSegments
            , capSegments: i.capSegments
            }
        )
        <|>
          ( map
              ( \(Capsule e) -> match
                  ( union
                      { radius: setRadius <<< { id: me, radius: _ }
                      , length: setLength <<< { id: me, length: _ }
                      , radialSegments: setRadialSegments <<<
                          { id: me, radialSegments: _ }
                      , capSegments: setCapSegments <<<
                          { id: me, capSegments: _ }
                      }
                      (C.bufferGeometry me di)
                  )
                  e
              )
              atts
          )

capsule_
  :: forall i lock payload
   . InitialCapsule i
  => i
  -> C.Geometry lock payload
capsule_ i = capsule i empty