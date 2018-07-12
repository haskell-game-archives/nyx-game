
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Credits where

import SDL.Vect (V4(..))
import qualified SDL
import qualified SDL.Mixer as Mix
import qualified Play.Engine.MySDL.MySDL as MySDL

import Play.Engine.Utils
import Play.Engine.Types
import Play.Engine.Input
import Play.Engine.Settings
import Control.Monad.Except
import Control.Lens
import System.Random
import qualified Play.Engine.State as State
import qualified Play.Engine.Load as Load
import qualified Control.Monad.State as SM

import qualified Script
import qualified Play.Engine.Sprite as Spr


data State
  = State
  { _bg :: Maybe Spr.Sprite
  , _script :: Script.Script
  , _camera :: !Int
  , _exit :: !Bool
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
  state <- initState (scrpt rs)
  pure $ State.mkState
    state
    update
    render

initState :: Script.Script -> Result State
initState scrpt = do
  pure $ State
    { _bg = Nothing
    , _script = scrpt
    , _camera = 0
    , _exit = False
    }

update :: Input -> State -> Result (State.Command, State)
update input state = do
  _wSize <- _windowSize <$> SM.get

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
                & over (bg . _Just)
                ( case Script.changeSprite acts of
                    Nothing -> Spr.update Nothing False
                    Just sp -> const sp
                )

  if
    | keyReleased KeyQuit input -> do
      pure (State.None, set exit True state)
    | state ^. exit -> do
      pure (State.Done, state)
    | otherwise ->
      pure (Script.command acts, newState)

render :: SDL.Renderer -> State -> IO ()
render renderer state = do
  cam' <- Point <$> randomRIO (-1, 1) <*> randomRIO (-1, 1) :: IO FPoint
  let cam = addPoint $ fmap (floor . (*) (fromIntegral $ state ^. camera `div` 3)) cam'
  void $ MySDL.setBGColor (V4 0 0 0 255) renderer
  case state ^. bg of
    Nothing -> pure ()
    Just bgSpr ->
      Spr.render renderer cam (Point 0 0) (bgSpr ^. size) 255 bgSpr

  Script.render renderer cam (state ^. script)

  when (state ^. exit) $
    Mix.pauseMusic
