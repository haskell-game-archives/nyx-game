
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module KeyScreen where

import Data.Maybe (fromJust)
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
import qualified Play.Engine.Sprite as Spr
import qualified Play.Engine.ListZipper as Z
import qualified Play.Engine.State as State
import qualified Play.Engine.Load as Load
import qualified Control.Monad.State as SM

import qualified Button as Btn


data State
  = State
  { _bg :: Spr.Sprite
  , _buttons :: Z.ListZipper (Btn.Button, Result State.Command)
  }

makeFieldsNoPrefix ''State

wantedAssets :: [(String, MySDL.ResourceType FilePath)]
wantedAssets =
  [ ("keys", MySDL.Texture "keys.png")
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
  case M.lookup "keys" (MySDL.textures rs) of
    Nothing ->
      throwError ["Texture not found: keys"]
    Just bgt -> do
      let
        makeBtn' n =
          Btn.make (Point 320 (720 + n * 60)) (Point 180 50) rs

      btns <- sequence $ zipWith (flip ($)) [0..] $
        [ \n -> (, pure $ State.Done)
          <$> makeBtn' n "Back"
        ]

      pure $ State
        { _bg =
          fromJust
            $ Spr.make
            $ Spr.MakeArgs
            { mkActionmap = ["normal"]
            , mkAction = "normal"
            , mkTexture = bgt
            , mkSize = Point 800 1000
            , mkMaxPos = 1
            , mkSpeed = 1
            }
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
  cmd <- bool (pure State.None) cmd' (check || keyClicked KeyQuit input)
  pure
    ( cmd
    , state
      & set buttons (fmap (first snd) btns)
      & over bg (Spr.update Nothing False)
    )

render :: SDL.Renderer -> State -> IO ()
render renderer state = do
  Spr.render renderer id (Point 0 0) (state ^. bg . size) (state ^. bg)
  shade renderer id 30
  void $ Z.diffMapM
    (Btn.render renderer False)
    (Btn.render renderer True)
    (fmap fst $ state ^. buttons)
