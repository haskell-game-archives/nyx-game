
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TemplateHaskell #-}

module GameState where

import qualified SDL
import qualified SDL.Font as SDLF
import qualified SDL.Mixer as Mix
import qualified Play.Engine.MySDL.MySDL as MySDL

import Play.Engine
import Control.Monad
import Control.Monad.Except
import Control.Lens
import Data.Maybe
import Data.Foldable
import System.Random
import qualified Play.Engine.Load as Load

import qualified Control.Monad.State as SM
import qualified Data.DList as DL
import qualified Data.Map as M

import qualified Script
import Bullet hiding (update, render)
import qualified Bullet
import qualified ShootingBox as SB
import qualified Enemy as Enemy
import qualified DecorationObject as DO
import qualified Play.Engine.ScrollingBackground as SBG
import qualified Play.Engine.Sprite as Spr


data State
  = State
  { _bg :: SBG.SBG
  , _bga :: Spr.Sprite
  , _mc :: SB.MainChar
  , _enemies :: [Enemy.Enemy]
  , _mcBullets :: DL.DList Bullet
  , _enemyBullets :: DL.DList Bullet
  , _decObjs :: DL.DList DO.DecorationObject
  , _resources :: MySDL.Resources
  , _script :: Script.Script
  , _camera :: Int
  , _restart :: Scene
  , _isPause :: !Bool
  , _pauseChanged :: !Bool
  , _isMute :: !Bool
  , _hudFont :: SDLF.Font
  , _exit :: !Bool
  }

makeFieldsNoPrefix ''State

wantedAssets :: [(String, MySDL.ResourceType FilePath)]
wantedAssets =
  [ ("bg", MySDL.Texture "background.png")
  , ("unispace", MySDL.Font "unispace/unispace.ttf")
  --, ("bga", MySDL.Texture "bga.png")
  ]
  ++ SB.wantedAssets

mkGameState :: Script.ScriptData -> Scene
mkGameState sd = Load.mkState 30 (wantedAssets ++ Script.assets sd) (mkState sd)

mkState
  :: Script.ScriptData
  -> MySDL.Resources -> Result Scene
mkState sd rs = do
  state <- initState sd (Script.script sd rs) rs
  pure $ mkScene
    state
    update
    render

initState :: Script.ScriptData -> Script.Script -> MySDL.Resources -> Result State
initState sd scrpt rs = do
  case (,) <$> M.lookup "bg" (MySDL.textures rs) <*> M.lookup "unispace" (MySDL.fonts rs) of
    Nothing ->
      throwError ["Texture or font not found: bg / unispace"]
    Just (bgt, font) -> do
      mc' <- (SB.mkMainChar $ MySDL.textures rs)
      pure $ State
        (SBG.mkSBG bgt 1 (Point 800 1000) (Point 0 0))
        ( maybe undefined id $ Spr.make $ Spr.MakeArgs
          { mkActionmap = ["normal"]
          , mkAction = "normal"
          , mkTexture = bgt
          , mkSize = Point 800 1000
          , mkMaxPos = 4
          , mkSpeed = 2
          }
        )
        mc'
        []
        (DL.fromList [])
        (DL.fromList [])
        (DL.fromList [])
        rs
        scrpt
        0
        (Script.restart sd)
        False
        False
        False
        font
        False

initEnemyTimer :: Int
initEnemyTimer = 60

update :: Input -> State -> Result (StackCommand, State)
update input state = do
  wSize <- _windowSize <$> SM.get
  ismute <- _muteMusic <$> SM.get

  (acts, script') <- Script.update input (SB.get (state ^. mc) pos) (state ^. enemies) (state ^. script)

  (mc', addMCBullets) <-
    SB.update
      (maybe input (dirToInput . dirToPlace (state ^. mc . pos)) (Script.moveMC acts))
      (state ^. mc)

  (enemies', addEnemiesBullets, newDecObjs) <-
    (\(a, b, c) ->
       ( mconcat a
       , foldr (.) id b
       , foldr (\curr acc -> DL.fromList curr `DL.append` acc) DL.empty c
       )
    ) . unzip3
      <$> traverse (Enemy.update input) (state ^. enemies)
  let
    (mcBullets', _enemiesHit) =
      updateListWith
        M.empty
        (const $ const M.empty)
        (Bullet.update wSize isTouchingCircleRect (state ^. enemies))
        (state ^. mcBullets)

    (enemyBullets', (concat . map snd . M.toList) -> mcHits) =
      updateListWith
        M.empty
        M.union
        (Bullet.update wSize isTouchingCircleRect $ maybe [] (:[]) $ SB.get (state ^. mc) id)
        (state ^. enemyBullets)

  updatedDecObjs <- updateDecObjs input (state ^. decObjs)

  let
    newState =
      state'
        & set pauseChanged False
        & set isMute ismute
        & set script script'
        & over bg SBG.updateSBG
        & over camera
          (\c ->
             if
               | c <= 0 && Script.shake acts -> 60
               | c <= 0 -> 0
               | otherwise -> c - 1
          )
        & over bga
          ( case Script.changeSprite acts of
              Nothing -> Spr.update Nothing False
              Just sp -> const sp
          )
      where
        state' =
          if Script.stopTheWorld acts
            then
              state
                & set mcBullets mcBullets'
                & set enemyBullets enemyBullets'
            else
              state
                & set mc (SB.checkHit mcHits mc')
                & set enemies enemies'
                & over enemies ((++) (Script.spawn acts) . map (Enemy.checkHit mcBullets'))
                & set mcBullets (addMCBullets mcBullets')
                & set enemyBullets (addEnemiesBullets enemyBullets')
                & set decObjs (updatedDecObjs `DL.append` newDecObjs)
  if
    | keyReleased KeyC input -> do
      pure (Replace $ state ^. restart, state)
    | keyReleased KeyP input && state ^. isPause -> do
      pure (None, set pauseChanged True $ set isPause False state)
    | keyReleased KeyP input && not (state ^. isPause) -> do
      pure (None, set pauseChanged True $ set isPause True state)
    | state ^. isPause -> do
      pure (None, state)
    | keyReleased KeyQuit input -> do
      pure (None, set exit True state)
    | state ^. exit -> do
      pure (Done, state)
    | otherwise ->
      pure (Script.command acts, newState)


updateDecObjs :: Input -> DL.DList DO.DecorationObject -> Result (DL.DList DO.DecorationObject)
updateDecObjs input =
  foldrM
    ( \(DO.DecObj obj@(DO.DecObj'{..})) acc -> do
      (r, objs) <- _update input _state
      pure $ DL.append
        (DL.fromList $ maybeToList (fmap (DO.DecObj . flip (set DO.state) obj) r) ++ objs)
        acc
    )
    DL.empty


flipEnemyDir :: Either () () -> Either () ()
flipEnemyDir = \case
  Right () -> Left ()
  Left () -> Right ()

render :: SDL.Renderer -> State -> IO ()
render renderer state = do

  cam' <- Point <$> randomRIO (-1, 1) <*> randomRIO (-1, 1) :: IO FPoint
  let cam = addPoint $ fmap (floor . (*) (fromIntegral $ state ^. camera `div` 3)) cam'
  SBG.render renderer cam (state ^. bg)
  --Spr.render renderer cam (Point 0 0) (state ^. bga . size) (state ^. bga)
  SB.render renderer cam (state ^. mc)
  void $ traverse (Enemy.render renderer cam) (state ^. enemies)
  forM_ (state ^. decObjs) (\(DO.DecObj (DO.DecObj'{..})) -> _render renderer cam _state)
  forM_ (state ^. mcBullets) (Bullet.render renderer cam)
  forM_ (state ^. enemyBullets) (Bullet.render renderer cam)

  when (state ^. isMute) $
    renderText renderer (state ^. hudFont) (Point 40 30) "MUTED"

  Script.render renderer cam (state ^. script)

  when (state ^. isPause)
    $ renderText renderer (state ^. hudFont) (Point 370 380) "PAUSE"

  when (state ^. pauseChanged) $
    if (state ^. isPause)
      then
        Mix.pauseMusic
      else
        Mix.resumeMusic

  when (state ^. exit) $
    Mix.pauseMusic

dirToInput :: IPoint -> Input
dirToInput dir =
  Input ks []
  where
    ks = M.fromList $ map (,Click)
      $  (if dir ^. x > 0 then [KeyRight] else if dir ^. x < 0 then [KeyLeft] else [])
      ++ (if dir ^. y > 0 then [KeyUp]    else if dir ^. y < 0 then [KeyDown] else [])

