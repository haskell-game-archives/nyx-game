{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Play.Engine.Input where

import Data.Maybe
import Data.Word (Word8)
import Data.Tuple
import qualified SDL
import qualified Play.Engine.MySDL.MySDL as MySDL
import Play.Engine.Types
import qualified Data.Map as M
import GHC.Generics
import Control.DeepSeq

--import Debug.Trace

data Input
  = Input
  { inputKeys :: !Keys
  , responses :: ![MySDL.Response]
  }

type Keys = M.Map Key Action

data Action
  = Click
  | Hold
  | Release
  | Idle
  deriving (Show, Eq, Ord, Generic, NFData)

data Key
  = KeyUp
  | KeyDown
  | KeyLeft
  | KeyRight
  | KeyA
  | KeyB
  | KeyC
  | KeyD
  | KeyM
  | KeyQuit
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Generic, NFData)

empty :: Input
empty = Input mempty mempty

initKeyStats :: Keys
initKeyStats = M.fromList $ zip [minBound..maxBound] (cycle [Idle])

defKeyMap :: [(Key, SDL.Scancode)]
defKeyMap = map swap
  [ (SDL.ScancodeW, KeyUp)
  , (SDL.ScancodeS, KeyDown)
  , (SDL.ScancodeA, KeyLeft)
  , (SDL.ScancodeD, KeyRight)
  , (SDL.ScancodeUp, KeyUp)
  , (SDL.ScancodeDown, KeyDown)
  , (SDL.ScancodeLeft, KeyLeft)
  , (SDL.ScancodeRight, KeyRight)
  , (SDL.ScancodeEscape, KeyQuit)
  , (SDL.ScancodeQ, KeyQuit)
  , (SDL.ScancodeZ, KeyA)
  , (SDL.ScancodeX, KeyB)
  , (SDL.ScancodeC, KeyC)
  , (SDL.ScancodeV, KeyD)
  , (SDL.ScancodeM, KeyM)
  ]

-- can't have more than one binding to the same key as this will create a state accumulation problem
defControllerButtonMap :: [(Key, Word8)]
defControllerButtonMap = map swap
  [ (13, KeyUp)
  , (14, KeyDown)
  , (11, KeyLeft)
  , (12, KeyRight)
  , (0, KeyB)
  , (5, KeyA)
  , (3, KeyC)
  ]

keepState :: Maybe Action -> Bool
keepState state
  | state == Just Click = True
  | state == Just Hold = True
  | state == Just Release = False
  | state == Just Idle = False

  | otherwise = False

checkControllerEvent :: Word8 -> SDL.EventPayload -> Maybe Bool
checkControllerEvent btn = \case
  SDL.JoyButtonEvent (SDL.JoyButtonEventData _ btn' SDL.JoyButtonPressed)
    | btn == btn' -> pure True
  SDL.JoyButtonEvent (SDL.JoyButtonEventData _ btn' SDL.JoyButtonReleased)
    | btn == btn' -> pure False
  SDL.JoyAxisEvent (SDL.JoyAxisEventData _ btn' _)
    | btn == btn' -> pure True
  _ -> Nothing

makeEvents :: Keys -> [SDL.EventPayload] -> (SDL.Scancode -> Bool) -> [(Key, SDL.Scancode)] -> Keys
makeEvents !current payload !isKeyPressed =
  let
    keyboard =
      fmap (fmap isKeyPressed)
    controller =
      fmap
        (\(key, btn) ->
          ( key
          ,
            let
              es = mapMaybe (checkControllerEvent btn) payload
            in
              if null es
                then keepState (M.lookup key current)
                else any id es
          )
        )
      $ defControllerButtonMap
  in
    updateKeys current
    . M.fromListWith max
    . (controller ++)
    . keyboard

updateKeys :: Keys -> M.Map Key Bool -> Keys
updateKeys !keys !newStates =
  flip M.mapWithKey keys $ \k s ->
    case (s, testKey k newStates) of
      (Idle, True) -> Click
      (Click, True) -> Hold
      (Hold, True) -> Hold
      (Release, True) -> Click
      (Idle, False) -> Idle
      (Click, False) -> Release
      (Hold, False) -> Release
      (Release, False) -> Idle

testKey :: Key -> M.Map Key Bool -> Bool
testKey key = maybe False id . M.lookup key

keyReleased :: Key -> Input -> Bool
keyReleased key = keyReleased' key . inputKeys

keyClicked :: Key -> Input -> Bool
keyClicked key = keyClicked' key . inputKeys

keyPressed :: Key -> Input -> Bool
keyPressed key = keyPressed' key . inputKeys

keyIdle :: Key -> Input -> Bool
keyIdle key = keyIdle' key . inputKeys

keyReleased' :: Key -> Keys -> Bool
keyReleased' key = maybe False (== Release) . M.lookup key

keyClicked' :: Key -> Keys -> Bool
keyClicked' key = maybe False (== Click) . M.lookup key

keyPressed' :: Key -> Keys -> Bool
keyPressed' key = maybe False (/= Idle) . M.lookup key

keyIdle' :: Key -> Keys -> Bool
keyIdle' key = maybe False (== Idle) . M.lookup key

keysToMovement :: Float -> Input -> FPoint
keysToMovement speed keys =
  let
      singleMove k1 k2
        | keyPressed k1 keys && not (keyPressed k2 keys) = -speed
        | keyPressed k2 keys && not (keyPressed k1 keys) =  speed
        | otherwise = 0
      hori = singleMove KeyUp KeyDown
      vert = singleMove KeyLeft KeyRight
  in Point vert hori
