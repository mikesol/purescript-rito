module Rito.Example.BMS where

import Prelude

import Affjax.ResponseFormat (string)
import Affjax.StatusCode (StatusCode(..))
import Affjax.Web as AX
import BMS.Parser (bms)
import BMS.Timing (gatherAll, noteOffsets)
import BMS.Types (Note(..), Offset(..))
import Bolson.Core (Child(..), envy)
import Control.Alt ((<|>))
import Control.Parallel (parTraverse)
import Control.Plus (empty)
import Data.Array (span)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Compactable (compact)
import Data.DateTime.Instant (unInstant)
import Data.Either (Either(..))
import Data.Foldable (fold, oneOf, oneOfMap, traverse_)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int as Int
import Data.Lens (_1, over)
import Data.List (List(..), drop, nub, null, take)
import Data.List as DL
import Data.Map (SemigroupMap(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Number (cos, pi, pow, sin)
import Data.String as String
import Data.Traversable (sequence)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typelevel.Num (D2)
import Deku.Attribute (attr, cb, (:=))
import Deku.Control (switcher, text, text_)
import Deku.DOM as D
import Deku.Toplevel (runInBody)
import Effect (Effect, foreachE)
import Effect.Aff (Aff, error, forkAff, launchAff_, throwError)
import Effect.Class (liftEffect)
import Effect.Class.Console as Log
import Effect.Now (now)
import Effect.Ref (new)
import Effect.Ref as Ref
import Effect.Timer (clearInterval, setInterval)
import FRP.Behavior (Behavior, behavior, sampleBy, step)
import FRP.Behavior.Time (instant)
import FRP.Event (Event, bang, filterMap, hot, makeEvent, mapAccum, subscribe)
import FRP.Event as Event
import FRP.Event.AnimationFrame (animationFrame)
import FRP.Event.VBus (V, vbus)
import Foreign (Foreign)
import Foreign.Object as Object
import Rito.Cameras.PerspectiveCamera (perspectiveCamera)
import Rito.Color (RGB(..))
import Rito.Core (toGroup, toScene)
import Rito.Geometries.Sphere (sphere)
import Rito.Group (group)
import Rito.Lights.PointLight (pointLight)
import Rito.Materials.MeshStandardMaterial (meshStandardMaterial)
import Rito.Mesh (mesh)
import Rito.Properties (color, positionX, positionY, positionZ, render, scaleX, scaleY, scaleZ, size)
import Rito.Renderers.WebGL (webGLRenderer)
import Rito.Run as Rito.Run
import Rito.Scene (scene)
import Simple.JSON as JSON
import Type.Proxy (Proxy(..))
import WAGS.Clock (withACTime)
import WAGS.Control (gain_, playBuf)
import WAGS.Core (Audible, dyn, silence, sound)
import WAGS.Interpret (close, constant0Hack, context, decodeAudioDataFromUri)
import WAGS.Math (calcSlope)
import WAGS.Properties as P
import WAGS.Run (run2)
import WAGS.WebAPI (AudioContext, BrowserAudioBuffer)
import Web.Event.Event (target)
import Web.HTML (window)
import Web.HTML.HTMLCanvasElement (HTMLCanvasElement)
import Web.HTML.HTMLCanvasElement as HTMLCanvasElement
import Web.HTML.HTMLInputElement (fromEventTarget, valueAsNumber)
import Web.HTML.Window (cancelIdleCallback, innerHeight, innerWidth, requestIdleCallback)

twoPi = 2.0 * pi :: Number
type Span = { s :: Int, n :: Int, d :: Int }
type Hap = { part :: { begin :: Span, end :: Span }, value :: Foreign }

data Cycle

queryArc :: Cycle -> Number -> Number -> Array Hap
queryArc _ _ _ = []

type StartStop = V (start :: Unit, stop :: Effect Unit)
type CanvasInfo = { x :: Number, y :: Number } /\ Number
type UIEvents = V
  ( startStop :: StartStop
  , slider :: Number
  , animationFrame :: Number
  , toAnimate :: Animated
  )

speed = 5.0 :: Number
runThree
  :: (Number -> Effect Unit -> Effect Unit)
  -> Event Animated
  -> Event Number
  -> Number
  -> Number
  -> HTMLCanvasElement
  -> Effect Unit
runThree lps e afE iw ih canvas = do
  _ <- Rito.Run.run
    ( webGLRenderer
        ( scene empty
            ( [ toScene $ pointLight { intensity: 1.0 }
                  ( oneOfMap bang
                      [ positionX 1.0
                      , positionY (-0.5)
                      , positionZ 1.0
                      ]
                  )
              , toScene $ dyn $
                  map
                    ( \nea ->
                        ( ( bang
                              ( Insert
                                  $ group
                                    ( oneOfMap
                                        bang
                                        [ positionX 0.0
                                        , positionY 0.0
                                        , positionZ 0.0
                                        ]
                                    )
                                    ( map
                                        ( \itm -> toGroup $ mesh
                                            ( sphere
                                                { widthSegments: 32
                                                , heightSegments: 32
                                                }
                                                empty
                                            )
                                            ( meshStandardMaterial
                                                { color: RGB 0.0 0.0 0.0
                                                , metalness: 0.0
                                                }
                                                ( afE <#>
                                                    ( \t ->
                                                        let
                                                          c = min 1.0 $ max 0.0
                                                            $ calcSlope
                                                              (itm.time - 1.0)
                                                              0.0
                                                              itm.time
                                                              1.0
                                                              t
                                                        in
                                                          color (RGB c c c)
                                                    )
                                                )
                                            )
                                            ( let
                                                s =
                                                  if itm.time < 2.0 then 0.0
                                                  else 0.1
                                              in
                                                oneOfMap
                                                  bang
                                                  [ positionX
                                                      (sin (324.124 * itm.time))
                                                  , positionY
                                                      ( cos
                                                          (1928.532 * itm.time)
                                                      )
                                                  , positionZ
                                                      (-1.0 * speed * itm.time)
                                                  , scaleX $ s
                                                  , scaleY $ s
                                                  , scaleZ $ s
                                                  ]
                                            )
                                        )
                                        (NEA.toArray nea)
                                    )
                              )
                          )
                        ) <|>
                          ( lowPrioritySchedule lps
                              ((NEA.last nea).removeAt)
                              (bang Remove)
                          )
                    )
                    e
              ]
            )

        )
        ( perspectiveCamera
            { fov: 75.0
            , aspect: iw / ih
            , near: 0.1
            , far: 100.0
            }
            ( oneOf
                [ bang (positionX 0.0)
                , bang (positionY 0.0)
                , positionZ <$> (map (negate >>> mul speed >>> add 2.0) afE)
                ]
            )
        )
        { canvas }
        ( bang (size { width: iw, height: ih }) <|> bang render <|>
            (afE $> render)
        )
    )
  pure unit

lowPrioritySchedule
  :: (Number -> Effect Unit -> Effect Unit) -> Number -> Event ~> Event
lowPrioritySchedule f n e = makeEvent \k -> do
  void $ subscribe e \i ->
    f n (k i)
  pure (pure unit)

type Animated = NEA.NonEmptyArray
  { removeAt :: Number
  , time :: Number
  , buffer :: BrowserAudioBuffer
  }

animate
  :: Behavior (Object.Object BrowserAudioBuffer)
  -> Behavior Number
  -> Map.Map Offset (List Note)
  -> Event Number
  -> Event Animated
animate babB clengthB offsetMap afE = compact
  $ map (NEA.fromArray <<< Array.fromFoldable)
  $
    ( (map <<< filterMap)
        ( \{ time, buffer, removeAt } -> case buffer of
            Just bf -> Just $ { time, buffer: bf, removeAt }
            Nothing -> Nothing
        )
    )
  $ mapAccum
    ( \{ behaviors: { clength, bab, tnow }, acTime }
       { writeAdj, prevACTime, prevAdjTime } -> do
        let prevAC = fromMaybe 0.0 prevACTime
        let prevAJ = fromMaybe 0.0 prevAdjTime
        let gap = acTime - prevAC
        let adjGap = gap / clength
        let adjTime = adjGap + prevAJ
        let lookAhead = 0.3
        let
          f wa =
            if wa < adjTime + lookAhead then
              ( let
                  wa1 = wa + 1.0
                  q /\ r = f wa1
                in
                  q
                    /\
                      ( Cons
                          ( let
                              haps = Map.submap (Just $ Offset wa)
                                (Just $ Offset wa1)
                                offsetMap
                            in
                              join $ Map.values $ mapWithIndex
                                ( \(Offset offset) -> mapWithIndex
                                    \i (Note n) ->
                                      { removeAt: (unwrap $ unInstant tnow)
                                          + 5000.0
                                          + Int.toNumber i
                                      , buffer: Object.lookup n bab
                                      , time: calcSlope prevAJ prevAC adjTime
                                          acTime
                                          offset
                                      }
                                )
                                haps
                          )
                          r
                      )
              )
            else wa /\ Nil
        let w /\ a = f writeAdj
        { writeAdj: w, prevACTime: Just acTime, prevAdjTime: Just adjTime } /\
          join a
    )
    ( sampleBy { behaviors: _, acTime: _ }
        ({ clength: _, bab: _, tnow: _ } <$> clengthB <*> babB <*> instant)
        (afE)
    )
    { writeAdj: 0.0, prevACTime: Nothing, prevAdjTime: Nothing }

midi2cps :: Int -> Number
midi2cps i = 440.0 * (2.0 `pow` (((Int.toNumber i) - 69.0) / 12.0))

graph
  :: forall lock payload
   . (Number -> Effect Unit -> Effect Unit)
  -> Event Animated
  -> Array (Audible D2 lock payload)
graph lps e =
  [ gain_ 1.0
      [ dyn $
          map
            ( \nea ->
                ( ( bang
                      ( sound
                          ( gain_ 1.0
                              ( NEA.toArray $ map
                                  ( \{ time, buffer } -> playBuf
                                      buffer
                                      (bang (P.onOff time))
                                  )
                                  nea
                              )
                          )
                      )
                  )
                    <|>
                      ( lowPrioritySchedule lps ((NEA.last nea).removeAt)
                          (bang silence)
                      )
                )
            )
            e
      ]
  ]

cvsx :: Int
cvsx = 1200
cvsxs :: String
cvsxs = show cvsx <> "px"
cvsxn :: Number
cvsxn = Int.toNumber cvsx
cvsy :: Int
cvsy = 600
cvsys :: String
cvsys = show cvsy <> "px"
cvsyn :: Number
cvsyn = Int.toNumber cvsy

dlInChunks
  :: Int
  -> List (Note /\ String)
  -> AudioContext
  -> Ref.Ref (Object.Object BrowserAudioBuffer)
  -> Aff Unit
dlInChunks i l ac rf = go i ixd
  where
  ixd = mapWithIndex (/\) l
  go a b = do
    let { init, rest } = DL.span (fst >>> (_ < a)) b
    o <- parTraverse
      ( sequence <<< map
          ( decodeAudioDataFromUri ac <<< (slashSilentRoomSlash <> _) <<<
              String.replace (String.Pattern ".wav") (String.Replacement ".ogg")
          )
      )
      (map (over _1 unwrap <<< snd) init)
    let asObj = Object.fromFoldable o
    liftEffect $ Ref.modify_ (Object.union asObj) rf
    when (not $ null rest) do
      go (a + i) rest

r2b :: Ref.Ref ~> Behavior
r2b r = behavior \e -> makeEvent \k -> subscribe e \f -> Ref.read r >>=
  (k <<< f)

slashSilentRoomSlash = "/silentroom/" :: String

main :: Effect Unit
main = launchAff_ do
  w <- liftEffect $ window
  iw <- liftEffect $ Int.toNumber <$> innerWidth w
  ih <- liftEffect $ Int.toNumber <$> innerHeight w
  soundObj <- liftEffect $ Ref.new Object.empty
  icid <- liftEffect $ new Nothing
  loaded <- liftEffect $ Event.create
  unschedule <- liftEffect $ new Map.empty
  ctx' <- liftEffect $ context
  bmsRes <- AX.get string (slashSilentRoomSlash <> "01.bme") >>= case _ of
    Left e -> throwError (error $ AX.printError e)
    Right r -> do
      when (r.status > StatusCode 300) do
        throwError (error $ show r)
      pure (bms r.body)
  let
    info = gatherAll bmsRes
    noffsets = noteOffsets info
    -- list of notes in the order we need them
    folded :: Map.Map Offset (List Note)
    folded = unwrap
      $ fold
      $ map SemigroupMap
      $ (map <<< map) pure
      $ Map.values noffsets
    n2o = compact
      $ map (sequence <<< ((/\) <*> flip Map.lookup info.headers.wavs))
      $ nub
      $ join
      $ Map.values
      $ folded
  let
    unsched k v = Ref.modify_ (Map.insert k v)
      unschedule

  Log.info $ JSON.writeJSON
    ( ( map
          (\((Offset a) /\ b) -> { o: a, n: Array.fromFoldable $ map unwrap b })
          :: _ -> Array _
      ) $ Map.toUnfoldable folded
    )
  -- _ <- never
  -- we just let this run & never kill it
  let n2oh = take 250 n2o
  let n2ot = drop 250 n2o
  _ <- forkAff do
    dlInChunks 25 n2oh ctx' soundObj
    liftEffect $ loaded.push true
    dlInChunks 25 n2ot ctx' soundObj
  liftEffect $ runInBody
    ( (loaded.event <|> bang false) # switcher case _ of
        false -> D.div_ [ D.h1_ [text_ "Loading (should take less than 10s)"] ]
        true ->
          (envy $ vbus (Proxy :: _ UIEvents) \push event ->
              do
                let
                  startE = bang unit <|> event.startStop.start
                  stopE = event.startStop.stop

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
                                    ( runThree
                                        unsched
                                        event.toAnimate
                                        event.animationFrame
                                        iw
                                        ih
                                    )
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
                              , ( oneOfMap
                                    (map (attr D.OnClick <<< cb <<< const))
                                    [ stopE <#>
                                        ( _ *> push.startStop.start unit
                                        )
                                    , startE <#> \_ -> do
                                        ctx <- context
                                        hk <- constant0Hack ctx
                                        ci <- setInterval 5000 do
                                          Ref.read icid >>= traverse_
                                            (flip cancelIdleCallback w)
                                          requestIdleCallback { timeout: 0 }
                                            ( do
                                                n <- unwrap <<< unInstant <$>
                                                  now
                                                mp <- Map.toUnfoldable <$>
                                                  Ref.read
                                                    unschedule
                                                let
                                                  lr = span (fst >>> (_ < n)) mp
                                                foreachE lr.init snd
                                                Ref.write
                                                  (Map.fromFoldable lr.rest)
                                                  unschedule
                                                pure unit
                                            )
                                            w <#> Just >>= flip Ref.write icid
                                        afE <- hot
                                          ( withACTime ctx animationFrame <#>
                                              _.acTime
                                          )
                                        animated <- hot
                                          ( animate (r2b soundObj)
                                              ( step 1.0
                                                  ( map
                                                      ( calcSlope 0.0 0.75 100.0
                                                          1.25
                                                      )
                                                      event.slider
                                                  )
                                              )
                                              folded
                                              afE.event
                                          )
                                        iu0 <- subscribe afE.event
                                          push.animationFrame
                                        iu1 <- subscribe animated.event
                                          push.toAnimate
                                        st <- run2 ctx
                                          ( graph
                                              unsched
                                              animated.event
                                          )
                                        push.startStop.stop
                                          ( st *> hk *> clearInterval ci
                                              *> afE.unsubscribe
                                              *> iu0
                                              *> iu1
                                              *> animated.unsubscribe
                                              *> close ctx
                                          )
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
    )