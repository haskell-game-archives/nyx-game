
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module PickStage where

import Data.Maybe (fromJust)
import qualified SDL
import qualified Play.Engine.MySDL.MySDL as MySDL

import Play.Engine hiding (head)
import Control.Monad.Except
import Control.Lens
import Data.Bifunctor
import Data.Bool
import qualified Data.Map as M
import qualified Play.Engine.Sprite as Spr
import qualified Play.Engine.ListZipper as Z
import qualified Play.Engine.Load as Load
import qualified Control.Monad.State as SM

import qualified Script.Introduction as Intro
import qualified Script.Level1 as Level1
import qualified Script.Level2 as Level2
import qualified Script.Boss as Boss
import qualified Script.End as End
import qualified Play.Engine.Button as Btn


data State
  = State
  { _bg :: Spr.Sprite
  , _buttons :: Z.ListZipper (Btn.Button, Result StackCommand)
  }

makeFieldsNoPrefix ''State

wantedAssets :: [(String, MySDL.ResourceType FilePath)]
wantedAssets =
  [ ("vnbg", MySDL.Texture "VNBG.png")
  , ("unispace", MySDL.Font "unispace/unispace.ttf")
  ]

make :: Scene
make = Load.mkState 0 wantedAssets mkState

mkState :: MySDL.Resources -> Result Scene
mkState rs = do
  state <- initState rs
  pure $ mkScene
    state
    update
    render

initState :: MySDL.Resources -> Result State
initState rs = do
  case (,)
    <$> M.lookup "vnbg" (MySDL.textures rs)
    <*> M.lookup "unispace" (MySDL.fonts rs) of
    Nothing ->
      throwError ["Texture not found: vnbg or unispace"]
    Just (bgt, fnt) -> do
      let
        makeBtn' n =
          Btn.make (Point 320 (600 + n * 60)) (Point 180 50) fnt

        makeBtn name state n =
          (makeBtn' n name, pure $ Replace state)
            

        btns = zipWith (flip ($)) [0..] $
          [ makeBtn "Intro" Intro.intro
          , makeBtn "Level1" $ Level1.level1 True
          , makeBtn "Level2" $ Level2.level2 True
          , makeBtn "Boss" $ Boss.boss True 0
          , makeBtn "Credits" $ End.end True

          , \n -> (makeBtn' n "Back", pure $ Done)
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
            , mkMaxPos = 8
            , mkSpeed = 8
            }
        , _buttons = Z.ListZipper
          []
          (head btns)
          (tail btns)
        }

update :: Input -> State -> Result (StackCommand, State)
update input state = do
  _wSize <- _windowSize <$> SM.get
  btns <- Z.diffMapM (firstM $ Btn.update empty) (firstM $ Btn.update input)
    $ if
       | keyClicked KeyDown input ->
         Z.nextCycle (state ^. buttons)

       | keyClicked KeyUp input ->
         Z.prevCycle (state ^. buttons)

        | otherwise ->
          state ^. buttons

  let ((check, _), cmd') = Z.get btns
  cmd <- bool (pure None) cmd' (check || keyClicked KeyQuit input)
  pure
    ( cmd
    , state
      & set buttons (fmap (first snd) btns)
      & over bg (Spr.update Nothing False)
    )

render :: SDL.Renderer -> State -> IO ()
render renderer state = do
  Spr.render renderer id (Point 0 0) (state ^. bg . size) 255 (state ^. bg)
  shade renderer id 120
  void $ Z.diffMapM
    (Btn.render renderer False)
    (Btn.render renderer True)
    (fmap fst $ state ^. buttons)
