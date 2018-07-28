
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings  #-}

module Enemy.CrossDown where

import qualified SDL
import qualified Play.Engine.MySDL.MySDL as MySDL
import qualified Data.Map as M
import Control.Monad.Except
import Control.Lens
import Data.Maybe (fromJust)

import Play.Engine

import Bullet
import Enemy
import qualified Attack as A
import qualified Attack.SpiralAttack as SA
import qualified Play.Engine.Movement as MV
import qualified Play.Engine.Sprite as Spr


wantedAssets :: [(String, MySDL.ResourceType FilePath)]
wantedAssets =
  [ ("bullet", MySDL.Texture "bullet.png")
  , ("crossdown", MySDL.Texture "crossdown.png")
  ]

make :: IPoint -> Either () () -> M.Map String SDL.Texture -> Result Enemy
make posi dir ts = do
  let textName = "bullet"
  case (,) <$> M.lookup textName ts <*> M.lookup "crossdown" ts of
    Nothing ->
      throwError ["Texture not found: crossdown or " ++ textName]
    Just (bt, et) -> do
      let
        sz = Point 48 48
      pure . mkEnemy $
        MakeEnemy
          { mkePos = posi
          , mkeSize = sz
          , mkeMov = crossMovement dir
          , mkeHealth = 70
          , mkeDirChanger = changeDirection
          , mkeAtk = downAttack bt
          , mkeAtkChanger = \_ _ -> Nothing
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
          , mkeDeathTime = 20
          , mkeDeathParts = Point 3 3
          , mkeDeathTexture = et
          }

crossMovement :: Either () () -> MV.Movement
crossMovement dir = MV.make $ MV.defArgs
  { MV.maxspeed = Point 4.5 2.5
  , MV.accel = Point (mul 0.1) 0.3
  }
  where
    mul = case dir of
      Left () -> (*) (-1)
      Right () -> (*) 1

downAttack :: SDL.Texture -> A.Attack
downAttack = SA.make 1 90 (12, 0) $ MV.straight (Point 0 8)

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

