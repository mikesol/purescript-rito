module Rito.BlendSrc where

data BlendSrc
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
  | SrcAlphaSaturateFactor