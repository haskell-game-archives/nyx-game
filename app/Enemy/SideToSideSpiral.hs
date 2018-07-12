
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings  #-}

module Enemy.SideToSideSpiral where

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
import qualified Play.Engine.Sprite as Spr
import Data.Maybe (fromJust)


wantedAssets :: [(String, MySDL.ResourceType FilePath)]
wantedAssets =
  [ ("static", MySDL.Texture "static.png")
  , ("crossdown", MySDL.Texture "crossdown.png")
  ]

make :: IPoint -> M.Map String SDL.Texture -> Result Enemy
make posi ts = do
  case (,) <$> M.lookup "static" ts <*> M.lookup "crossdown" ts of
    Nothing ->
      throwError ["Texture not found: static or crossdown" ]
    Just (et, bt) -> do
      let
        sz = Point 48 48
      pure . mkEnemy $
        MakeEnemy
          { mkePos = posi
          , mkeSize = sz
          , mkeMov = leftRightMovement
          , mkeHealth = 100
          , mkeDirChanger = changeDirection
          , mkeAtk = singleSpiralAttack bt
          , mkeAtkChanger = \_ _ -> Nothing
          , mkeDeathTime = 0
          , mkeDeathParts = Point 3 3
          , mkeSprite =
            fromJust
              $ Spr.make
              $ Spr.MakeArgs
              { mkActionmap = ["normal"]
              , mkAction = "normal"
              , mkTexture = et
              , mkSize = sz
              , mkMaxPos = 1
              , mkSpeed = 1
              }
          , mkeHitbox = Hitbox
            { _alignment = Point 4 4
            , _size = sz
              & over x (\n -> n - 8)
              & over y (\n -> n - 8)
            }
          }

leftRightMovement :: MV.Movement
leftRightMovement = MV.make $ MV.MakeArgs
  { MV.startspeed = Point 0 0
  , MV.minspeed = Point 0 0
  , MV.maxspeed = Point 1.5 1.5
  , MV.accel = Point 0.1 0.1
  }

singleSpiralAttack :: SDL.Texture -> A.Attack
singleSpiralAttack = SA.make 6 0 (2, 1.2) $ MV.gradualSlowdown (Point 8 8)

changeDirection :: Size -> Enemy -> FPoint
changeDirection wsize enemy
  | enemy ^. direction . x == 0
  , enemy ^. direction . y == 0
  = Point 0 1

  | enemy ^. pos . y >= 100
  , enemy ^. direction . y == 1
  = Point 1 0

  | enemy ^. pos . x > 2 * (wsize ^. x `div` 3) - enemy ^. sprite . size . x
  , enemy ^. direction . y == 0
  = Point (-1) 0

  | enemy ^. pos . x <= (wsize ^. x `div` 3)
  , enemy ^. direction . y == 0
  = Point 1 0

  | enemy ^. health <= 0
  = Point 0 0

  | otherwise
  = enemy ^. direction

