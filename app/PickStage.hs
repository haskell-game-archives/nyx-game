
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module PickStage where

import SDL.Vect (V4(..))
import qualified SDL
import qualified Play.Engine.MySDL.MySDL as MySDL

import Play.Engine.Utils hiding (head)
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
import qualified Script.Level1 as Level1
import qualified Script.Level2 as Level2
import qualified Script.Boss as Boss
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
      let
        makeBtn' n =
          Btn.make (Point 320 (600 + n * 60)) (Point 180 50) rs

        makeBtn name state n =
          (, pure $ State.Replace state)
            <$> makeBtn' n name

      btns <- sequence $ zipWith (flip ($)) [0..] $
        [ makeBtn "Intro" Intro.intro
        , makeBtn "Level1" Level1.level1
        , makeBtn "Level2" Level2.level2
        , makeBtn "Boss" Boss.boss

        , \n -> (, pure $ State.Done)
          <$> makeBtn' n "Back"
        ]

      pure $ State
        { _background = bgt
        , _buttons = Z.ListZipper
          []
          (head btns)
          (tail btns)
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
  void $ MySDL.setBGColor (V4 50 0 30 255) renderer
  void $ Z.diffMapM
    (Btn.render renderer False)
    (Btn.render renderer True)
    (fmap fst $ state ^. buttons)
