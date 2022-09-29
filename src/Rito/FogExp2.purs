module Rito.FogExp2 where


import Effect (Effect)
import Rito.Color (Color)
import Rito.THREE as THREE

data FogExp2

foreign import fogExp2 :: THREE.TFogExp2 -> Color -> Number -> Effect FogExp2