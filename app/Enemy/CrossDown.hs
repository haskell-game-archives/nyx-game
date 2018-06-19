
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TemplateHaskell #-}

module Enemy.CrossDown where

import qualified SDL
import qualified Play.Engine.MySDL.MySDL as MySDL
import qualified Data.Map as M
import Control.Monad.Except
import Control.Lens

import Play.Engine.Utils
import Play.Engine.Types
import Play.Engine.Settings

import Bullet
import Enemy
import qualified Attack as A
import qualified Attack.SpiralAttack as SA
import qualified Play.Engine.Movement as MV


wantedAssets :: [(String, MySDL.ResourceType FilePath)]
wantedAssets =
  [ ("moon", MySDL.Texture "moon2.png")
  , ("crossdown", MySDL.Texture "crossdown.png")
  ]

make :: IPoint -> Either () () -> M.Map String SDL.Texture -> Result Enemy
make posi dir ts = do
  let textName = "moon"
  case (,) <$> M.lookup textName ts <*> M.lookup "crossdown" ts of
    Nothing ->
      throwError ["Texture not found: crossdown or " ++ textName]
    Just (bt, et) ->
      pure . mkEnemy $
        MakeEnemy
          { mkePos = posi
          , mkeSize = Point 48 48
          , mkeMov = crossMovement dir
          , mkeHealth = 100
          , mkeDirChanger = changeDirection
          , mkeAtk = downAttack bt
          , mkeAtkChanger = \_ _ -> Nothing
          , mkeEnemyTxt = et
          , mkeDeathTime = 20
          }

crossMovement :: Either () () -> MV.Movement
crossMovement dir = MV.make $ MV.defArgs
  { MV.maxspeed = Point 4 2.5
  , MV.accel = Point (mul 0.1) 0.3
  }
  where
    mul = case dir of
      Left () -> (*) (-1)
      Right () -> (*) 1

downAttack :: SDL.Texture -> A.Attack
downAttack = SA.make 1 90 (10, 0) $ MV.straight (Point 0 8)

changeDirection :: Size -> Enemy -> FPoint
changeDirection _ enemy
  | enemy ^. direction . x == 0
  , enemy ^. direction . y == 0
  = Point 0 1

  | enemy ^. pos . y >= 0
  , enemy ^. direction . y == 1
  , enemy ^. direction . x == 0
  = Point 1 1

  | otherwise
  = enemy ^. direction

