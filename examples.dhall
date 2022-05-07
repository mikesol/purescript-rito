let conf = ./spago.dhall

in      conf
    //  { sources = conf.sources # [ "examples/**/*.purs" ]
        , dependencies =
              conf.dependencies
            # [ "web-html"
              , "canvas"
              , "filterable"
              , "affjax-web"
              , "arraybuffer"
              , "affjax"
              , "tuples"
              , "either"
              , "integers"
              , "behaviors"
              , "deku"
              , "wags"
              , "aff"
              , "argonaut-core"
              , "web-uievents"
              , "http-methods"
              , "maybe"
              , "typelevel"
              , "arrays"
              , "homogeneous"
              , "lcg"
              , "parallel"
              , "quickcheck"
              , "uint"
              ]
        }
