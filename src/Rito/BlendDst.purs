module Rito.BlendDst where

data BlendDst
  = ZeroFactor
  | OneFactor
  | SrcColorFactor
  | OneMinusSrcColorFactor
  | SrcAlphaFactor
  | OneMinusSrcAlphaFactor
  | DstAlphaFactor
  | OneMinusDstAlphaFactor
  | DstColorFactor
  | OneMinusDstColorFactor
