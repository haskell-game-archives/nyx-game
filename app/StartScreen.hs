
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module StartScreen where

import SDL.Vect (V4(..))
import qualified SDL
import qualified Play.Engine.MySDL.MySDL as MySDL

import Play.Engine.Utils
import Play.Engine.Types
import Play.Engine.Input as I
import Play.Engine.Settings
--import Control.Monad
import Control.Monad.Except
import Control.Lens
import Data.Bifunctor
import Data.Bool
import qualified Data.Map as M
import qualified Play.Engine.ListZipper as Z
import qualified Play.Engine.State as State
import qualified Play.Engine.Load as Load
import qualified Control.Monad.State as SM

import qualified Script.Introduction as Intro
import qualified PickStage as Pick
import qualified Button as Btn


data State
  = State
  { _background :: SDL.Texture
  , _buttons :: Z.ListZipper (Btn.Button, Result State.Command)
  }

makeFieldsNoPrefix ''State

wantedAssets :: [(String, MySDL.ResourceType FilePath)]
wantedAssets =
  [ ("bg", MySDL.Texture "background.png")
  ] ++ Btn.wantedAssets

make :: State.State
make = Load.mkState 0 wantedAssets mkState

mkState :: MySDL.Resources -> Result State.State
mkState rs = do
  state <- initState rs
  pure $ State.mkState
    state
    update
    render

initState :: MySDL.Resources -> Result State
initState rs = do
  case M.lookup "bg" (MySDL.textures rs) of
    Nothing ->
      throwError ["Texture not found: bg"]
    Just bgt -> do
      startBtn <- Btn.make (Point 320 600) (Point 180 50) rs "Start"
      pickStageBtn <- Btn.make (Point 320 660) (Point 180 50) rs "Pick Stage"
      exitBtn <- Btn.make (Point 320 720) (Point 180 50) rs "Exit"
      pure $ State
        { _background = bgt
        , _buttons = Z.ListZipper
          []
          (startBtn, pure $ State.Push Intro.intro)
          [ (pickStageBtn, pure $ State.Push Pick.make)
          , (exitBtn, throwError [])
          ]
        }

update :: Input -> State -> Result (State.Command, State)
update input state = do
  _wSize <- _windowSize <$> SM.get
  btns <- Z.diffMapM (firstM $ Btn.update I.empty) (firstM $ Btn.update input)
    $ if
       | keyClicked KeyDown input ->
         Z.nextCycle (state ^. buttons)

       | keyClicked KeyUp input ->
         Z.prevCycle (state ^. buttons)

        | otherwise ->
          state ^. buttons

  let ((check, _), cmd') = Z.get btns
  cmd <- bool (pure State.None) cmd' check
  pure (cmd, set buttons (fmap (first snd) btns) state)

render :: SDL.Renderer -> State -> IO ()
render renderer state = do
  void $ MySDL.setBGColor (V4 10 0 20 255) renderer
  void $ Z.diffMapM
    (Btn.render renderer False)
    (Btn.render renderer True)
    (fmap fst $ state ^. buttons)

