module Rito.CSS.CSS3DObject
  ( css3DObject
  , CSS3DObject(..)
  , CSS3DObject'
  ) where

import Prelude

import Bolson.Core as Bolson
import Control.Alt ((<|>))
import Control.Monad.ST (ST)
import Control.Monad.ST.Global as Region
import Data.Newtype (class Newtype)
import Data.Variant (Variant, match)
import Deku.Core (ANut(..))
import Deku.Toplevel (runInElement')
import Effect (Effect)
import FRP.Event (Event, makeLemmingEvent)
import Rito.Core as C
import Rito.THREE as THREE
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (Element)
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
   . { css3DObject :: THREE.TCSS3DObject, nut :: ANut }
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
      ) = makeLemmingEvent \mySub k -> do
    me <- ids
    parent.raiseId me
    --- UGGGGHHHHH
    elt <- (unsafeCoerce :: Effect Element -> ST Region.Global Element) (window >>= document >>= createElement "div" <<< toDocument)
    --- UGGGGHHHH
    dku <- ((unsafeCoerce :: Effect (Effect Unit) -> ST Region.Global (ST Region.Global Unit)) (runInElement' elt nut))
    unsub <- mySub
      ( pure
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
                (\(CSS3DObject e) -> match (C.object3D me di) e)
                atts
            )
      )
      k
    pure do
      k (deleteFromCache { id: me })
      -- ugggghhhhhhhhhh
      dku
      unsub
