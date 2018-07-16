
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings  #-}

module Enemy.Fast where

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
  [ ("bullet", MySDL.Texture "bullet.png")
  , ("astral-sprite", MySDL.Texture "astral-sprite.png")
  , ("astral", MySDL.Texture "astral.png")
  ]

make :: IPoint -> M.Map String SDL.Texture -> Result Enemy
make posi ts = do
  let textName = "bullet"
  case (,,) <$> M.lookup textName ts <*> M.lookup "astral" ts <*> M.lookup "astral-sprite" ts of
    Nothing ->
      throwError ["Texture not found: astral or " ++ textName ++ " in " ++ show (M.keys ts)]
    Just (bt, et, spr) -> do
      let
        sz = Point 100 100
      pure . mkEnemy $
        MakeEnemy
          { mkePos = posi
          , mkeSize = sz
          , mkeMov = crossMovement (Right ())
          , mkeHealth = 300
          , mkeDirChanger = changeDirection
          , mkeAtk = attackPattern1 bt
          , mkeAtkChanger = \enemy atkId ->
              if
                | enemy ^. health < 200 && atkId < 1 -> pure $ attackPattern2 bt
                | enemy ^. health < 100 && atkId < 2 -> pure $ attackPattern3 bt
                | otherwise -> Nothing

          , mkeSprite =
            fromJust
              $ Spr.make
              $ Spr.MakeArgs
              { mkActionmap = ["normal"]
              , mkAction = "normal"
              , mkTexture = spr
              , mkSize = sz
              , mkMaxPos = 4
              , mkSpeed = 5
              }
          , mkeHitbox = Hitbox
            { _alignment = Point 30 6
            , _size = sz
              & over x (\n -> n - 60)
              & over y (\n -> n - 12)
            }
          , mkeDeathTime = 150
          , mkeDeathParts = Point 4 3
          , mkeDeathTexture = et
          }

crossMovement :: Either () () -> MV.Movement
crossMovement dir = MV.make $ MV.defArgs
  { MV.maxspeed = Point 1.8 1
  , MV.accel = Point (mul 0.4) 0.2
  }
  where
    mul = case dir of
      Left () -> (*) (-1)
      Right () -> (*) 1

attackPattern1 :: SDL.Texture -> A.Attack
attackPattern1 = SA.make 5 0 (1, 9) $ MV.fastGradualStart (Point 3.2 3.2)

attackPattern2 :: SDL.Texture -> A.Attack
attackPattern2 = SA.make 3 0 (1, 7) $ MV.fastGradualStart (Point 3.2 3.2)

attackPattern3 :: SDL.Texture -> A.Attack
attackPattern3 = SA.make 5 0 (3, 7) $ MV.gradualSlowdown (Point 12 14)


changeDirection :: Size -> Enemy -> FPoint
changeDirection wsize enemy
  | enemy ^. direction . x == 0
  , enemy ^. direction . y == 0
  = Point 0 1

  | enemy ^. pos . y >= 0
  , enemy ^. direction . y == 1
  , enemy ^. direction . x == 0
  = Point 1 1

  | enemy ^. pos . y <= 0
  = enemy ^. direction

  | otherwise
  = enemy ^. direction
    & set x
      (if
         | enemy ^. pos . x > (wsize ^. x * 2) `div` 3 -> -1
         | enemy ^. pos . x <= wsize ^. x `div` 3 -> 1
         | otherwise -> enemy ^. direction . x
      )
    & set y
      (if
         | enemy ^. pos . y > 250 -> -1
         | enemy ^. pos . y <= 50 -> 1
         | otherwise -> enemy ^. direction . y
      )
