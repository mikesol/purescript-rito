module Rito.Geometries.Plane
  ( plane
  , plane_
  , Plane(..)
  , Plane'
  , class InitialPlane
  , PlaneOptions
  , toInitializePlane
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

data PlaneOptions = PlaneOptions

instance
  ConvertOption PlaneOptions
    "width"
    Number
    Number where
  convertOption _ _ = identity

instance
  ConvertOption PlaneOptions
    "height"
    Number
    Number where
  convertOption _ _ = identity

instance
  ConvertOption PlaneOptions
    "widthSegments"
    Int
    Int where
  convertOption _ _ = identity

instance
  ConvertOption PlaneOptions
    "heightSegments"
    Int
    Int where
  convertOption _ _ = identity

type PlaneOptional =
  ( width :: Number
  , height :: Number
  , widthSegments :: Int
  , heightSegments :: Int
  )

type PlaneAll =
  (| PlaneOptional)

defaultPlane :: { | PlaneOptional }
defaultPlane =
  { width: 1.0
  , height: 1.0
  , widthSegments: 1
  , heightSegments: 1
  }

class InitialPlane i where
  toInitializePlane :: i -> C.InitializePlane

instance InitialPlane C.InitializePlane where
  toInitializePlane = identity

instance
  ConvertOptionsWithDefaults PlaneOptions { | PlaneOptional } { | provided }
    { | PlaneAll } =>
  InitialPlane { | provided } where
  toInitializePlane provided = C.InitializePlane
    (convertOptionsWithDefaults PlaneOptions defaultPlane provided)

type Plane' = Variant
      ( width :: Number
      , height :: Number
      , widthSegments :: Int
      , heightSegments :: Int
      | C.BufferGeometry
      )
newtype Plane = Plane Plane'
instance Newtype Plane Plane'

plane
  :: forall i lock payload
   . InitialPlane i
  => i
  -> Event Plane
  -> C.Geometry lock payload
plane i' atts = C.Geometry go
  where
  C.InitializePlane i = toInitializePlane i'
  go
    parent
    di@( C.ThreeInterpret
        { ids
        , deleteFromCache
        , makePlane
        , setWidth
        , setHeight
        , setWidthSegments
        , setHeightSegments
        }
    ) = makeEvent \k -> do
    me <- ids
    parent.raiseId me
    map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
      bang
        ( makePlane
            { id: me
            , parent: parent.parent
            , scope: parent.scope
            , width: i.width
            , height: i.height
            , widthSegments: i.widthSegments
            , heightSegments: i.heightSegments
            }
        )
        <|>
          ( map
              ( \(Plane e) -> match
                  (union { width: setWidth <<< { id: me, width: _ }
                  , height: setHeight <<< { id: me, height: _ }
                  , widthSegments: setWidthSegments <<<
                      { id: me, widthSegments: _ }
                  , heightSegments: setHeightSegments <<<
                      { id: me, heightSegments: _ }
                  } (C.bufferGeometry me di))
                  e
              )
              atts
          )

plane_
  :: forall i lock payload
   . InitialPlane i
  => i
  -> C.Geometry lock payload
plane_ i = plane i empty