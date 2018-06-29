
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
import qualified PickStage as Pick
import qualified Button as Btn


data State
  = State
  { _background :: SDL.Texture
  , _buttons :: Z.ListZipper (Btn.Button, Result State.Command)
  , _cheat :: !Int
  }

makeFieldsNoPrefix ''State

wantedAssets :: [(String, MySDL.ResourceType FilePath)]
wantedAssets =
  [ ("bg", MySDL.Texture "background.png")
  ] ++ Btn.wantedAssets

make :: State.State
make = Load.mkState 0 wantedAssets (mkState 5)

mkState :: Int -> MySDL.Resources -> Result State.State
mkState cheat_ rs = do
  state <- initState cheat_ rs
  pure $ State.mkState
    state
    update
    render

initState :: Int -> MySDL.Resources -> Result State
initState cheat_ rs = do
  case M.lookup "bg" (MySDL.textures rs) of
    Nothing ->
      throwError ["Texture not found: bg"]
    Just bgt -> do
      let
        makeBtn' n =
          Btn.make (Point 320 (600 + n * 60)) (Point 180 50) rs

        makeBtn name state n =
          (, pure $ State.Push state)
            <$> makeBtn' n name

      btns <- sequence $ zipWith (flip ($)) [0..] $
        [ makeBtn "Start" Intro.intro ]
        ++ [ makeBtn "Pick Stage" Pick.make | cheat_ <= 0 ]
        ++ [ \n -> (, throwError [])
             <$> makeBtn' n "Exit"
           ]

      pure $ State
        { _background = bgt
        , _buttons = Z.ListZipper
          []
          (head btns)
          (tail btns)
        , _cheat = cheat_
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
  pure
    ( if state ^. cheat == 0
        then State.Replace $ Load.mkState 0 wantedAssets (mkState $ -1)
        else cmd
    , state
      & set buttons (fmap (first snd) btns)
      & over cheat (if keyClicked KeyC input then (\c -> c - 1) else id)
    )

render :: SDL.Renderer -> State -> IO ()
render renderer state = do
  let n = fromIntegral $ max (-1) (state ^. cheat) * 4
  void $ MySDL.setBGColor (V4 (10 + n) 0 20 255) renderer
  void $ Z.diffMapM
    (Btn.render renderer False)
    (Btn.render renderer True)
    (fmap fst $ state ^. buttons)

