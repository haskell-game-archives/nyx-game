
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings  #-}

module Enemy.Static where

import qualified SDL
import qualified Play.Engine.MySDL.MySDL as MySDL
import qualified Data.Map as M
import Control.Monad.Except
import Control.Lens

import Play.Engine

import Bullet
import Enemy
import qualified Attack as A
import qualified Attack.SpiralAttack as SA
import qualified Play.Engine.Movement as MV
import qualified Play.Engine.Sprite as Spr
import Data.Maybe (fromJust)


wantedAssets :: [(String, MySDL.ResourceType FilePath)]
wantedAssets =
  [ ("bullet", MySDL.Texture "bullet.png")
  , ("static", MySDL.Texture "static.png")
  ]

make :: IPoint -> FPoint -> Int -> M.Map String SDL.Texture -> Result Enemy
make posi dir targetY ts = do
  let textName = "bullet"
  case (,) <$> M.lookup textName ts <*> M.lookup "static" ts of
    Nothing ->
      throwError ["Texture not found: static or " ++ textName ++ " in:\n" ++ show (M.keys ts)]
    Just (bt, et) -> do
      let
        sz = Point 48 48
      pure . mkEnemy $
        MakeEnemy
          { mkePos = posi
          , mkeSize = sz
          , mkeMov = staticMovement dir
          , mkeHealth = 5
          , mkeDirChanger = changeDirection targetY
          , mkeAtk = sprayAttack bt
          , mkeAtkChanger = \_ _ -> Nothing
          , mkeDeathTime = 30
          , mkeDeathParts = Point 3 3
          , mkeDeathTexture = et
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

staticMovement :: FPoint -> MV.Movement
staticMovement dir = MV.make $ MV.defArgs
  { MV.maxspeed = Point 3 3
  , MV.accel = Point (dir ^. x * 0.1) (dir ^.y * 0.1)
  }

sprayAttack :: SDL.Texture -> A.Attack
sprayAttack = SA.make 5 0 (45, 15) $ MV.straight (Point 3.5 3.5)

changeDirection :: Int -> Size -> Enemy -> FPoint
changeDirection targetY _ enemy
  | enemy ^. direction . x == 0
  , enemy ^. direction . y == 0
  , enemy ^. pos . y < targetY
  = Point 1 1

  | enemy ^. pos . y >= targetY
  , enemy ^. direction . y == 1
  = Point 0 0

  | otherwise
  = enemy ^. direction

