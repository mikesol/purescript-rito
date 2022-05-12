module Rito.Geometries.Sphere
  ( sphere
  , sphere_
  , Sphere(..)
  , Sphere'
  , class InitialSphere
  , SphereOptions
  , toInitializeSphere
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (empty)
import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Data.Newtype (class Newtype)
import Data.Number (pi)
import Data.Variant (Variant, match)
import FRP.Event (Event, bang, makeEvent, subscribe)
import Record (union)
import Rito.Core as C

twoPi = pi * 2.0 :: Number
data SphereOptions = SphereOptions

instance
  ConvertOption SphereOptions
    "radius"
    Number
    Number where
  convertOption _ _ = identity

instance
  ConvertOption SphereOptions
    "widthSegments"
    Int
    Int where
  convertOption _ _ = identity

instance
  ConvertOption SphereOptions
    "heightSegments"
    Int
    Int where
  convertOption _ _ = identity

instance
  ConvertOption SphereOptions
    "phiStart"
    Number
    Number where
  convertOption _ _ = identity

instance
  ConvertOption SphereOptions
    "phiLength"
    Number
    Number where
  convertOption _ _ = identity

instance
  ConvertOption SphereOptions
    "thetaStart"
    Number
    Number where
  convertOption _ _ = identity

instance
  ConvertOption SphereOptions
    "thetaLength"
    Number
    Number where
  convertOption _ _ = identity

type SphereOptional =
  ( radius :: Number
  , widthSegments :: Int
  , heightSegments :: Int
  , phiStart :: Number
  , phiLength :: Number
  , thetaStart :: Number
  , thetaLength :: Number
  )

type SphereAll =
  (| SphereOptional)

defaultSphere :: { | SphereOptional }
defaultSphere =
  { radius: 1.0
  , widthSegments: 32
  , heightSegments: 16
  , phiStart: zero
  , phiLength: twoPi
  , thetaStart: zero
  , thetaLength: twoPi
  }

class InitialSphere i where
  toInitializeSphere :: i -> C.InitializeSphere

instance InitialSphere C.InitializeSphere where
  toInitializeSphere = identity

instance
  ConvertOptionsWithDefaults SphereOptions { | SphereOptional } { | provided }
    { | SphereAll } =>
  InitialSphere { | provided } where
  toInitializeSphere provided = C.InitializeSphere
    (convertOptionsWithDefaults SphereOptions defaultSphere provided)

type Sphere' = Variant
      ( radius :: Number
      , widthSegments :: Int
      , heightSegments :: Int
      , phiStart :: Number
      , phiLength :: Number
      , thetaStart :: Number
      , thetaLength :: Number
      | C.BufferGeometry
      )
newtype Sphere = Sphere Sphere'
instance Newtype Sphere Sphere'

sphere
  :: forall i lock payload
   . InitialSphere i
  => i
  -> Event Sphere
  -> C.Geometry lock payload
sphere i' atts = C.Geometry go
  where
  C.InitializeSphere i = toInitializeSphere i'
  go
    parent
    di@( C.ThreeInterpret
        { ids
        , deleteFromCache
        , makeSphere
        , setRadius
        , setWidthSegments
        , setHeightSegments
        , setPhiStart
        , setPhiLength
        , setThetaStart
        , setThetaLength
        }
    ) = makeEvent \k -> do
    me <- ids
    parent.raiseId me
    map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
      bang
        ( makeSphere
            { id: me
            , parent: parent.parent
            , scope: parent.scope
            , radius: i.radius
            , widthSegments: i.widthSegments
            , heightSegments: i.heightSegments
            , phiStart: i.phiStart
            , phiLength: i.phiLength
            , thetaStart: i.thetaStart
            , thetaLength: i.thetaLength
            }
        )
        <|>
          ( map
              ( \(Sphere e) -> match
                  (union { radius: setRadius <<< { id: me, radius: _ }
                  , widthSegments: setWidthSegments <<<
                      { id: me, widthSegments: _ }
                  , heightSegments: setHeightSegments <<<
                      { id: me, heightSegments: _ }
                  , phiStart: setPhiStart <<< { id: me, phiStart: _ }
                  , phiLength: setPhiLength <<< { id: me, phiLength: _ }
                  , thetaStart: setThetaStart <<< { id: me, thetaStart: _ }
                  , thetaLength: setThetaLength <<< { id: me, thetaLength: _ }
                  } (C.bufferGeometry me di))
                  e
              )
              atts
          )

sphere_
  :: forall i lock payload
   . InitialSphere i
  => i
  -> C.Geometry lock payload
sphere_ i = sphere i empty