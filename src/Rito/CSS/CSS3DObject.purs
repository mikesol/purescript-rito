module Rito.CSS.CSS3DObject
  ( css3DObject
  , CSS3DObject(..)
  , CSS3DObject'
  ) where

import Prelude

import Bolson.EffectFn.Core as Bolson
import Control.Alt ((<|>))
import Data.Newtype (class Newtype)
import Data.Variant (Variant, match)
import Deku.Core (ANut(..))
import Deku.Toplevel (runInElement')
import FRP.Event.EffectFn (Event,  makeEvent, subscribe)
import Rito.Core as C
import Rito.THREE as THREE
import Web.DOM.Document (createElement)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document)

type CSS3DObject' = Variant
  ( -- object3D
  | C.Object3D
  )
newtype CSS3DObject = CSS3DObject CSS3DObject'
instance Newtype CSS3DObject CSS3DObject'

css3DObject
  :: forall lock payload
   .  { css3DObject :: THREE.TCSS3DObject, nut :: ANut }
  -> Event CSS3DObject
  -> C.ACSS3DObject lock payload
css3DObject ipt@{ nut: ANut nut } atts = Bolson.Element' $ C.CSS3DObject go
  where
  go
    parent
    di@
      ( C.ThreeInterpret
          { ids
          , deleteFromCache
          , makeCSS3DObject
          }
      ) = makeEvent \k -> do
    me <- ids
    parent.raiseId me
    elt <- window >>= document >>= createElement "div" <<< toDocument
    dku <- runInElement' elt nut
    map ((k (deleteFromCache { id: me }) *> dku) *> _) $ flip subscribe k $
      pure
        ( makeCSS3DObject
            { id: me
            , parent: parent.parent
            , scope: parent.scope
            , css3DObject: ipt.css3DObject
            , nut: elt
            }
        )
        <|>
          ( map
              ( \(CSS3DObject e) -> match (C.object3D me di) e              )
              atts
          )
