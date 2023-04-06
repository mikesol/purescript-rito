module Rito.CSS.CSS2DObject
  ( css2DObject
  , CSS2DObject(..)
  , CSS2DObject'
  ) where

import Prelude

import Bolson.Core as Bolson
import Control.Monad.ST.Class (liftST)
import Data.Foldable (oneOf)
import Data.Newtype (class Newtype)
import Data.Variant (Variant, match)
import Deku.Core (ANut(..))
import Deku.Toplevel (runInElement')
import FRP.Event (Event, makeEvent, subscribe)
import Rito.Core as C
import Rito.THREE as THREE
import Web.DOM.Document (createElement)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document)

type CSS2DObject' = Variant
  ( -- object3D
  | C.Object3D
  )
newtype CSS2DObject = CSS2DObject CSS2DObject'
instance Newtype CSS2DObject CSS2DObject'

css2DObject
  :: forall payload
   . { css2DObject :: THREE.TCSS2DObject, nut :: ANut }
  -> Event CSS2DObject
  -> C.ACSS2DObject payload
css2DObject ipt@{ nut: ANut nut } atts = Bolson.Element' $ C.CSS2DObject go
  where
  go
    parent
    di@
      ( C.ThreeInterpret
          { ids
          , deleteFromCache
          , makeCSS2DObject
          }
      ) = makeEvent \k -> do
    me <- liftST ids
    liftST $ parent.raiseId me
    elt <- window >>= document >>= createElement "div" <<< toDocument
    dku <- runInElement' elt nut
    unsub <- subscribe
      ( oneOf
          [ pure
              ( makeCSS2DObject
                  { id: me
                  , parent: parent.parent
                  , scope: parent.scope
                  , nut: elt
                  , css2DObject: ipt.css2DObject

                  }
              )
          , map
              (\(CSS2DObject e) -> match (C.object3D me di) e)
              atts
          ]
      )
      k
    pure do
      k (deleteFromCache { id: me })
      dku
      unsub
