module Main where

import Prelude

import Control.Alt ((<|>))
import Control.Parallel (parTraverse)
import Control.Plus (empty)
import Data.Array (zip, (!!), (..))
import Data.Array.NonEmpty (fromArray, singleton)
import Data.ArrayBuffer.Typed (toArray)
import Data.DateTime.Instant (unInstant)
import Data.FastVect.FastVect as FV
import Data.FastVect.Sparse.Read as FVR
import Data.Foldable (for_, oneOf, oneOfMap, traverse_)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Homogeneous.Record as Rc
import Data.Int as Int
import Data.Lens (over)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Number (cos, pi, sin, (%))
import Data.Profunctor.Strong (second)
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (toNumber)
import Deku.Attribute (attr, cb, (:=))
import Deku.Control (text)
import Deku.DOM as D
import Deku.Toplevel (runInBody1)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)
import Effect.Random as Random
import Effect.Ref (new, read, write)
import FRP.Behavior (behavior)
import FRP.Event (Event, bang, makeEvent, subscribe)
import FRP.Event.Animate (animationFrameEvent)
import FRP.Event.Time (withTime)
import FRP.Event.VBus (V, vbus)
import Foreign.Object (fromHomogeneous, values)
import Random.LCG (mkSeed)
import Rito.Cameras.PerspectiveCamera (defaultOrbitControls, perspectiveCamera)
import Rito.Color (RGB(..), color)
import Rito.Core (OrbitControls(..), toScene)
import Rito.Geometries.Sphere (sphere)
import Rito.InstancedMesh (instancedMesh, setter)
import Rito.Interpret (orbitControlsAff, threeAff)
import Rito.Lights.PointLight (pointLight)
import Rito.Materials.MeshStandardMaterial (meshStandardMaterial)
import Rito.Matrix4 (ctor, scale, setPosition)
import Rito.Properties (positionX, positionY, positionZ, render, setMatrixAt, size)
import Rito.Renderers.WebGL (webGLRenderer)
import Rito.Run as Rito.Run
import Rito.Scene (scene)
import Rito.THREE (ThreeStuff)
import Rito.Vector3 (vector3)
import Test.QuickCheck.Gen (elements, evalGen)
import Type.Proxy (Proxy(..))
import WAGS.Clock (withACTime)
import WAGS.Control (analyser_, bandpass, delay, fan1, fix, gain, gain_, highpass, lowpass, playBuf)
import WAGS.Core (Audible, AudioEnvelope(..), AudioNumeric(..), Po2(..), _linear, bangOn)
import WAGS.Interpret (close, constant0Hack, context, decodeAudioDataFromUri, getByteFrequencyData)
import WAGS.Math (calcSlope)
import WAGS.Properties as P
import WAGS.Run (run2)
import WAGS.WebAPI (AnalyserNodeCb(..))
import Web.Event.Event (target)
import Web.HTML (window)
import Web.HTML.HTMLCanvasElement (HTMLCanvasElement)
import Web.HTML.HTMLCanvasElement as HTMLCanvasElement
import Web.HTML.HTMLInputElement (fromEventTarget, valueAsNumber)
import Web.HTML.Window (innerHeight, innerWidth)

type StartStop = V (start :: Unit, stop :: Effect Unit)
type CanvasInfo = { x :: Number, y :: Number } /\ Number
type UIEvents = V
  ( startStop :: StartStop
  , slider :: Number
  , canvas :: Array CanvasInfo
  )

e2e :: Effect ~> Event
e2e e = makeEvent \k -> do
  e >>= k
  pure (pure unit)

buffers' =
  { pluck0: "https://freesound.org/data/previews/493/493016_10350281-lq.mp3"
  , pluck1: "https://freesound.org/data/previews/141/141524_2558140-lq.mp3"
  , strum0: "https://freesound.org/data/previews/234/234738_3635427-lq.mp3"
  --, bass: "https://freesound.org/data/previews/381/381517_7088365-lq.mp3"
  }

random = behavior \e ->
  makeEvent \k -> subscribe e \f ->
    Random.random >>= k <<< f

dgl d de g ge h he i =
  delay d de [ gain g ge [ lowpass h he i ] ]

dgh d de g ge h he i =
  delay d de [ gain g ge [ highpass h he i ] ]

dgb d de g ge h he i =
  delay d de [ gain g ge [ bandpass h he i ] ]

twoPi = 2.0 * pi :: Number

fade0 = bang
  $ P.gain
  $ AudioEnvelope
    { p: [ 1.0, 1.0, 0.75, 0.5, 0.75, 0.5, 0.75, 0.5, 0.25, 0.5, 0.25, 0.0 ]
    , o: 0.0
    , d: 24.0
    }

fade1 = bang
  $ P.gain
  $ AudioEnvelope
    { p: [ 1.0, 1.0, 0.75, 0.5, 0.75, 0.5, 0.75, 0.5, 0.25, 0.5, 0.25, 0.0 ]
    , o: 0.0
    , d: 18.0
    }

cvsx = 1200
cvsxs = show cvsx <> "px"
cvsxn = Int.toNumber cvsx
cvsy = 600
cvsys = show cvsy <> "px"
cvsyn = Int.toNumber cvsy
fenv s e = bang
  $ P.frequency
  $ AudioEnvelope { p: [ s, e ], o: 0.0, d: 16.0 }

denv s e = bang
  $ P.delayTime
  $ AudioEnvelope { p: [ s, e ], o: 0.0, d: 16.0 }

ttap (o /\ n) = AudioNumeric { o: o + 0.04, n, t: _linear }

runThree
  :: ThreeStuff
  -> Event (Array CanvasInfo)
  -> Number
  -> Number
  -> HTMLCanvasElement
  -> Effect Unit
runThree ts@{ three } canvas iw ih e = do
  _ <- Rito.Run.run ts
    ( webGLRenderer
        ( scene empty
            [ toScene $ pointLight { color: color three $ RGB 1.0 1.0 1.0 }
                ( oneOfMap bang
                    [ positionX 1.0
                    , positionY (-0.5)
                    , positionZ 1.0
                    ]
                )
            , toScene $ instancedMesh
                ( sphere { widthSegments: 32, heightSegments: 32 }
                    empty
                )
                ( meshStandardMaterial
                    { color: color three $ RGB 1.0 1.0 1.0
                    , metalness: 1.0
                    }
                    empty
                )
                (( over
                    (prop (Proxy :: Proxy "time"))
                    ( (_ / 1000.0)
                        <<< unwrap
                        <<< unInstant
                    ) <$> withTime (canvas <|> bang [])
                    <#>
                      \{ time
                       , value: a
                       } -> do
                        let f i = do
                              let tni = Int.toNumber i
                              let tni40 = tni / 40.0
                              fromMaybe ({ x: 0.0, y: 0.0 } /\ 0.0)
                                (a !! i) # \({ x, y } /\ n) -> do
                                scale
                                  ( vector3 three
                                      { x: (n * 0.1)
                                      , y: (n * 0.1)
                                      , z: (n * 0.1)
                                      }
                                  )
                                  ( setPosition
                                      ( vector3 three
                                          { x:
                                              sin
                                                ( 0.2 * tni40 * time *
                                                    twoPi
                                                )
                                                + x * 2.0
                                                - 1.0
                                          , y:
                                              cos
                                                ( 0.2 * tni40 * time *
                                                    twoPi
                                                )
                                                + y * 2.0
                                                - 1.0
                                          , z:
                                              ( if i `mod` 2 == 0 then cos
                                                else sin
                                              )
                                                ( 0.2 * tni40 * time *
                                                    twoPi
                                                )
                                                * 1.0
                                          }
                                      )
                                      (ctor three)
                                  )
                        if time % 1.0 < 0.25 then setMatrixAt $ (setter :: FV.Vect 40 _ -> _) $ mapWithIndex (#) $ pure f else setMatrixAt $ setter $ mapWithIndex (#)
                          $ FVR.set (Proxy :: _ 0) f
                          $ FVR.set (Proxy :: _ 2) f
                          $ FVR.set (Proxy :: _ 3) f
                          $ FVR.set (Proxy :: _ 11) f
                          $ FVR.set (Proxy :: _ 12) f
                          $ FVR.set (Proxy :: _ 19) f
                          $ FVR.set (Proxy :: _ 20) f
                          $ FVR.set (Proxy :: _ 21) f
                          $ FVR.set (Proxy :: _ 27) f
                          $ FVR.set (Proxy :: _ 28) f
                          $ FVR.set (Proxy :: _ 36) f
                          $ FVR.sparse
                ))
            ]
        )
        ( perspectiveCamera
            { fov: 75.0
            , aspect: iw / ih
            , near: 0.1
            , far: 100.0
            , orbitControls: OrbitControls ((defaultOrbitControls e) { enabled = true })
            }
            ( oneOf
                [ bang (positionX 0.0)
                , bang (positionY 0.0)
                , bang (positionZ 2.0)
                ]
            )
        )
        { canvas: e }
        ( bang (size { width: iw, height: ih }) <|> bang render <|>
            (canvas $> render)
        )
    )
  pure unit

main :: Effect Unit
main = launchAff_ do
  three <- threeAff
  orbitControls <- orbitControlsAff
  let threeStuff = { three, orbitControls }
  w <- liftEffect $ window
  iw <- liftEffect $ Int.toNumber <$> innerWidth w
  ih <- liftEffect $ Int.toNumber <$> innerHeight w
  ctx' <- context
  sounds <- Rc.fromHomogeneous <$> parTraverse
    (decodeAudioDataFromUri ctx')
    (Rc.homogeneous buffers')
  close ctx'
  liftEffect $ runInBody1
    ( vbus (Proxy :: _ UIEvents) \push event -> -- here

        do
          let
            startE = bang unit <|> event.startStop.start
            stopE = event.startStop.stop

            music :: forall lock. _ -> _ -> _ -> Array (Audible _ lock _)
            music ctx buffer analyserE = do
              let
                sliderE = (\{ acTime, value } -> acTime /\ value) <$> withACTime
                  ctx
                  event.slider
              [ analyser_
                  { cb:
                      ( AnalyserNodeCb
                          ( \a -> do
                              write (Just a) analyserE
                              pure (write Nothing analyserE)
                          )
                      )
                  , fftSize: TTT7
                  }
                  [ fan1
                      ( playBuf buffer
                          ( bangOn <|>
                              ( P.playbackRate <<< ttap <<< second
                                  (calcSlope 0.0 0.96 100.0 1.04)
                              ) <$> sliderE
                          )
                      )
                      \b _ -> fix
                        \g0 -> gain_ 1.0
                          [ b
                          , delay { maxDelayTime: 2.5, delayTime: 1.0 }
                              ( ( P.delayTime <<< ttap <<< second
                                    (calcSlope 0.0 0.5 100.0 2.45)
                                ) <$> sliderE
                              )
                              [ gain 0.4
                                  ( ( P.gain <<< ttap <<< second
                                        (calcSlope 0.0 0.6 100.0 0.9)
                                    ) <$> sliderE
                                  )
                                  [ b ]
                              ]
                          , dgh 0.15 empty 0.7 empty 1500.0 (fenv 1500.0 3000.0)
                              [ fix
                                  \g1 -> gain 1.0 fade1
                                    [ dgh 0.4 empty 0.5 empty 3000.0
                                        (fenv 3000.0 100.0)
                                        [ g0, g1 ]
                                    ]
                              ]
                          , dgh 0.29
                              ( ( P.delayTime <<< ttap <<< second
                                    (calcSlope 0.0 0.1 100.0 0.4)
                                ) <$> sliderE
                              ) {-(denv 0.29 0.9)-}
                              0.85
                              empty
                              2000.0
                              (fenv 2000.0 5000.0)
                              [ fix
                                  \g1 -> gain_ 1.0
                                    [ dgh 0.6
                                        ( ( P.delayTime <<< ttap <<< second
                                              (calcSlope 0.0 0.8 100.0 0.3)
                                          ) <$> sliderE
                                        ) {-(denv 0.6 0.2)-}
                                        0.6
                                        empty
                                        3500.0
                                        (fenv 3500.0 100.0)
                                        [ g0
                                        , ( fix
                                              \g2 -> gain 1.0 fade0
                                                [ dgb 0.75
                                                    ( ( P.delayTime <<< ttap <<<
                                                          second
                                                            ( calcSlope 0.0 0.9
                                                                100.0
                                                                0.1
                                                            )
                                                      ) <$> sliderE
                                                    )
                                                    0.6
                                                    empty
                                                    4000.0
                                                    (fenv 4000.0 200.0)
                                                    [ g1, g2 ]
                                                , dgb 0.75 (denv 0.75 0.2) 0.55
                                                    empty
                                                    200.0
                                                    (fenv 200.0 4000.0)
                                                    [ b ]
                                                ]
                                          )
                                        ]
                                    ]
                              ]
                          ]
                  ]
              ]
          D.div_
            [ D.div
                ( bang
                    ( D.Style :=
                        "position:absolute;"
                    )
                )
                [ D.canvas
                    ( oneOfMap bang
                        [ D.Width := cvsxs
                        , D.Height := cvsys
                        , D.Style := "width: 100%;"
                        , D.Self := HTMLCanvasElement.fromElement >>>
                            traverse_
                              (runThree threeStuff event.canvas iw ih)
                        ]
                    )
                    []
                ]
            , D.div
                ( bang $ D.Style :=
                    "position: absolute; width:100%; background-color: rgba(200,200,200,0.8);"
                )
                [ D.input
                    ( oneOfMap bang
                        [ D.Xtype := "range"
                        , D.Min := "0"
                        , D.Max := "100"
                        , D.Step := "1"
                        , D.Value := "50"
                        , D.Style :=
                            "width: 100%; margin-top: 15px; margin-bottom: 15px;"
                        , D.OnInput := cb
                            ( traverse_
                                (valueAsNumber >=> push.slider)
                                <<< (=<<) fromEventTarget
                                <<< target
                            )
                        ]
                    )
                    []
                , D.button
                    ( oneOf
                        [ bang $ D.Style :=
                            "width:100%; padding:1.0rem;"
                        , ( oneOfMap (map (attr D.OnClick <<< cb <<< const))
                              [ stopE <#>
                                  ( _ *> push.startStop.start unit
                                  )
                              , startE <#> \_ -> do
                                  analyserE <- new Nothing
                                  -- todo: fix race condition in case
                                  -- we turn off while loading
                                  ctx <- context
                                  c0h <- constant0Hack ctx
                                  ri <- liftEffect $ randomInt 0 50000
                                  let
                                    randSound = evalGen
                                      ( elements
                                          $ fromMaybe (singleton sounds.pluck0)
                                          $ fromArray
                                          $ values
                                          $ fromHomogeneous sounds
                                      )
                                      { newSeed: mkSeed ri, size: 4 }
                                  liftEffect do
                                    rands <- 0 .. 127 # traverse \_ -> do
                                      x <- Random.random
                                      y <- Random.random
                                      pure { x, y }
                                    ssub <- run2 ctx
                                      (music ctx randSound analyserE)
                                    anisub <- subscribe animationFrameEvent
                                      \_ -> do
                                        ae <- read analyserE
                                        for_ ae \a -> do
                                          frequencyData <-
                                            getByteFrequencyData a
                                          arr <- map
                                            ( zip rands <<< map
                                                ((_ / 255.0) <<< toNumber)
                                            )
                                            (toArray frequencyData)
                                          push.canvas arr
                                          pure unit
                                    let res = ssub *> c0h *> close ctx *> anisub
                                    push.startStop.stop res
                                  -- pure res
                                  pure unit
                              ]
                          )
                        ]
                    )
                    [ text $ oneOf
                        [ map (const "Turn off") stopE
                        , map (const "Turn on") startE
                        ]
                    ]
                ]
            ]
    )