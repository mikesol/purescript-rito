module Rito.DepthMode where

data DepthMode
  = NeverDepth
  | AlwaysDepth
  | EqualDepth
  | LessDepth
  | LessEqualDepth
  | GreaterEqualDepth
  | GreaterDepth
  | NotEqualDepth