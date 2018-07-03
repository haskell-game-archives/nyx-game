
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module VN where

import SDL.Vect (V4(..))
import qualified SDL
import qualified SDL.Mixer as Mix
import qualified SDL.Font as SDLF
import qualified Play.Engine.MySDL.MySDL as MySDL

import Play.Engine.Utils
import Play.Engine.Types
import Play.Engine.Input
import Play.Engine.Settings
import Data.Maybe
import Control.Monad.Except
import Control.Lens
import System.Random
import qualified Play.Engine.State as State
import qualified Play.Engine.Load as Load
import qualified Control.Monad.State as SM
import qualified Data.Map as M

import qualified Script
import qualified Play.Engine.Sprite as Spr


data State
  = State
  { _bg :: Spr.Sprite
  , _resources :: MySDL.Resources
  , _script :: Script.Script
  , _camera :: Int
  , _isPause :: !Bool
  , _pauseChanged :: !Bool
  , _isMute :: !Bool
  , _hudFont :: SDLF.Font
  }

makeFieldsNoPrefix ''State

wantedAssets :: [(String, MySDL.ResourceType FilePath)]
wantedAssets =
  [ ("bg", MySDL.Texture "VNBG.png")
  , ("unispace", MySDL.Font "unispace/unispace.ttf")
  ]

make :: Int -> Script.ScriptData -> State.State
make t sd = Load.mkState t (wantedAssets ++ Script.assets sd) (mkState $ Script.script sd)

mkState
  :: (MySDL.Resources -> Script.Script)
  -> MySDL.Resources -> Result State.State
mkState scrpt rs = do
  state <- initState (scrpt rs) rs
  pure $ State.mkState
    state
    update
    render

initState :: Script.Script -> MySDL.Resources -> Result State
initState scrpt rs = do
  case (,) <$> M.lookup "bg" (MySDL.textures rs) <*> M.lookup "unispace" (MySDL.fonts rs) of
    Nothing ->
      throwError ["Texture not found: bg ot unispace"]
    Just (bgt, font) -> do
      pure $ State
        { _bg =
          fromJust
            $ Spr.make
            $ Spr.MakeArgs
            { mkActionmap = ["normal"]
            , mkAction = "normal"
            , mkTexture = bgt
            , mkSize = Point 800 1000
            , mkMaxPos = 8
            , mkSpeed = 8
            }
        , _resources = rs
        , _script = scrpt
        , _camera = 0
        , _isPause = False
        , _pauseChanged = False
        , _isMute = False
        , _hudFont = font
        }

update :: Input -> State -> Result (State.Command, State)
update input st = do
  _wSize <- _windowSize <$> SM.get
  ismute <- _muteMusic <$> SM.get
  let
    state = set isMute ismute st

  (acts, script') <- Script.update input Nothing mempty (state ^. script)

  let
    newState =
      state'
        & set script script'
        & over camera
          (\c ->
             if
               | c <= 0 && Script.shake acts -> 60
               | c <= 0 -> 0
               | otherwise -> c - 1
          )
      where
        state' =
          if Script.stopTheWorld acts
            then
              state
            else
              state
                & over bg
                ( case Script.changeSprite acts of
                    Nothing -> Spr.update Nothing False
                    Just sp -> const sp
                )

  if
    | keyReleased KeyP input && state ^. isPause -> do
      pure (State.None, set pauseChanged True $ set isPause False state)
    | keyReleased KeyP input && not (state ^. isPause) -> do
      pure (State.None, set pauseChanged True $ set isPause True state)
    | state ^. isPause -> do
      pure (State.None, state)
    | keyReleased KeyQuit input -> do
      pure (State.Done, state)
    | otherwise ->
      pure (Script.command acts, newState)

render :: SDL.Renderer -> State -> IO ()
render renderer state = do
  cam' <- Point <$> randomRIO (-1, 1) <*> randomRIO (-1, 1) :: IO FPoint
  let cam = addPoint $ fmap (floor . (*) (fromIntegral $ state ^. camera `div` 3)) cam'
  Spr.render renderer cam (Point 0 0) (state ^. bg . size) (state ^. bg)

  when (state ^. isMute) $
    renderText renderer (state ^. hudFont) (Point 40 30) "MUTED"

  Script.render renderer cam (state ^. script)
  renderText renderer (state ^. hudFont) (Point 900 30) "MUTED"

  when (state ^. isPause) $ do
    let rect = toRect (cam $ Point 342 375) (Point 85 35)
    SDL.rendererDrawColor renderer SDL.$= V4 40 0 30 180
    SDL.fillRect renderer (Just rect)
    renderText renderer (state ^. hudFont) (Point 352 380) "PAUSE"

  when (state ^. pauseChanged) $
    if (state ^. isPause)
      then
        Mix.pauseMusic
      else
        Mix.resumeMusic

