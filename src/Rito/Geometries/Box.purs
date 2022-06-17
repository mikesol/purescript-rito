module Rito.Geometries.Box
  ( box
  , box_
  , Box(..)
  , Box'
  , class InitialBox
  , BoxOptions
  , toInitializeBox
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
import Rito.THREE as THREE

data BoxOptions = BoxOptions

instance
  ConvertOption BoxOptions
    "box"
    THREE.TBoxGeometry
    THREE.TBoxGeometry where
  convertOption _ _ = identity

instance
  ConvertOption BoxOptions
    "width"
    Number
    Number where
  convertOption _ _ = identity

instance
  ConvertOption BoxOptions
    "height"
    Number
    Number where
  convertOption _ _ = identity

instance
  ConvertOption BoxOptions
    "depth"
    Number
    Number where
  convertOption _ _ = identity

instance
  ConvertOption BoxOptions
    "widthSegments"
    Int
    Int where
  convertOption _ _ = identity

instance
  ConvertOption BoxOptions
    "heightSegments"
    Int
    Int where
  convertOption _ _ = identity

instance
  ConvertOption BoxOptions
    "depthSegments"
    Int
    Int where
  convertOption _ _ = identity

type BoxOptional =
  ( width :: Number
  , height :: Number
  , depth :: Number
  , widthSegments :: Int
  , heightSegments :: Int
  , depthSegments :: Int
  )

type BoxAll =
  (box :: THREE.TBoxGeometry | BoxOptional)

defaultBox :: { | BoxOptional }
defaultBox =
  { width: 1.0
  , height: 1.0
  , depth: 1.0
  , widthSegments: 1
  , heightSegments: 1
  , depthSegments: 1
  }

class InitialBox i where
  toInitializeBox :: i -> C.InitializeBox

instance InitialBox C.InitializeBox where
  toInitializeBox = identity

instance
  ConvertOptionsWithDefaults BoxOptions { | BoxOptional } { | provided }
    { | BoxAll } =>
  InitialBox { | provided } where
  toInitializeBox provided = C.InitializeBox
    (convertOptionsWithDefaults BoxOptions defaultBox provided)

type Box' = Variant
  ( width :: Number
  , height :: Number
  , depth :: Number
  , widthSegments :: Int
  , heightSegments :: Int
  , depthSegments :: Int
  | C.BufferGeometry
  )
newtype Box = Box Box'
instance Newtype Box Box'

box
  :: forall i lock payload
   . InitialBox i
  => i
  -> Event Box
  -> C.Geometry lock payload
box i' atts = C.Geometry go
  where
  C.InitializeBox i = toInitializeBox i'
  go
    parent
    di@
      ( C.ThreeInterpret
          { ids
          , deleteFromCache
          , makeBox
          , setWidth
          , setHeight
          , setDepth
          , setWidthSegments
          , setHeightSegments
          , setDepthSegments
          }
      ) = makeEvent \k -> do
    me <- ids
    parent.raiseId me
    map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
      bang
        ( makeBox
            { id: me
            , parent: parent.parent
            , scope: parent.scope
            , box: i.box
            , width: i.width
            , height: i.height
            , depth: i.depth
            , widthSegments: i.widthSegments
            , heightSegments: i.heightSegments
            , depthSegments: i.depthSegments
            }
        )
        <|>
          ( map
              ( \(Box e) -> match
                  ( union
                      { width: setWidth <<< { id: me, width: _ }
                      , height: setHeight <<< { id: me, height: _ }
                      , depth: setDepth <<< { id: me, depth: _ }
                      , widthSegments: setWidthSegments <<<
                          { id: me, widthSegments: _ }
                      , heightSegments: setHeightSegments <<<
                          { id: me, heightSegments: _ }
                      , depthSegments: setDepthSegments <<<
                          { id: me, depthSegments: _ }
                      }
                      (C.bufferGeometry me di)
                  )
                  e
              )
              atts
          )

box_
  :: forall i lock payload
   . InitialBox i
  => i
  -> C.Geometry lock payload
box_ i = box i empty