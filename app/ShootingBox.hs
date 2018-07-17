{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module ShootingBox where

import qualified SDL
import qualified Play.Engine.MySDL.MySDL as MySDL

import Play.Engine.Utils
import Play.Engine.Types
import Play.Engine.Input
import Play.Engine.Settings
import Data.Maybe
import Control.Monad.Except
import Control.Lens
import Control.DeepSeq
import qualified Control.Monad.State as SM
import qualified Data.DList as DL
import qualified Data.Map as M

import qualified Play.Engine.Movement as MV
import qualified Play.Engine.Sprite as Spr
import Bullet

-- import Debug.Trace



data Dir
  = DirRight
  | DirLeft
  deriving (Eq, Show)

data MainChar
  = MainChar
  { _pos :: {-# UNPACK #-} !IPoint
  , _size :: {-# UNPACK #-} !Size
  , _movement :: {-# UNPACK #-} !MV.Movement
  , _sprite :: !Spr.Sprite
  , _bullet :: !SDL.Texture
  , _hitTimer :: {-# UNPACK #-} !Int
  , _bulletsTimer :: {-# UNPACK #-} !Int
  , _health :: {-# UNPACK #-} !Int
  , _lastDir :: !Dir
  , _hitbox :: !Hitbox
  }

makeFieldsNoPrefix ''MainChar

instance NFData MainChar where
  rnf (MainChar {_pos, _size, _hitTimer, _movement, _health, _bulletsTimer}) =
    rnf _pos
    `seq` rnf _size
    `seq` rnf _direction
    `seq` rnf _movement
    `seq` rnf _hitTimer
    `seq` rnf _bulletsTimer
    `seq` rnf _health
    `seq` rnf _transparency

instance Eq MainChar where
  mc1 == mc2 =
    mc1 ^. pos == mc2 ^. pos
    && mc1 ^. size == mc2 ^. size

instance Ord MainChar where
  mc1 <= mc2 =
    mc1 ^. pos <= mc2 ^. pos
    && mc1 ^. size <= mc2 ^. size

wantedAssets :: [(String, MySDL.ResourceType FilePath)]
wantedAssets =
  [ ("nyx-sprites", MySDL.Texture "nyx-sprites-animated.png")
  , ("nyx-bullet", MySDL.Texture "nyx-bullet.png")
  ]


mkMainChar :: M.Map String SDL.Texture -> Result MainChar
mkMainChar ts = do
  case mapM ((`M.lookup` ts) . fst) wantedAssets of
    Just [nyxSprites, nyxBullet] ->
      pure $
        MainChar
          { _pos = Point 380 800
          , _size = charSize
          , _sprite =
            fromJust
              $ Spr.make
              $ Spr.MakeArgs
              { mkActionmap = ["normal", "side-right", "side-left"]
              , mkAction = "normal"
              , mkTexture = nyxSprites
              , mkSize = Point 180 380
              , mkMaxPos = 4
              , mkSpeed = 5
              }
          , _bullet = nyxBullet
          , _hitTimer = -1
          , _bulletsTimer = 5
          , _health = 1
          , _movement = MV.make $ MV.defArgs
            { MV.maxspeed = Point 5 5
            , MV.accel = Point 3.5 3.5
            }
          , _lastDir = DirRight
          , _hitbox = Hitbox
            { _alignment = Point (charSize ^. x `div` 4) (charSize ^. y `div` 4)
            , _size = charSize
              & over x (`div` 2)
              & over y (`div` 2)
            }
          }
    _ ->
      throwError ["Texture not found: nyx-sprites"]

charSize :: Size
charSize = Point 60 108

fixHitpos :: MainChar -> MainChar
fixHitpos mc = mc
  & set (hitbox . alignment . x) (charSize ^. x `div` 4)
  & set (hitbox . alignment . y) (charSize ^. y `div` 4)
  & set (hitbox . size . x) (charSize ^. x `div` 2)
  & set (hitbox . size . y) (charSize ^. y `div` 3)

halfHitbox :: MainChar -> MainChar
halfHitbox mc = mc
  & set (hitbox . alignment . x) (charSize ^. x `div` 3)
  & set (hitbox . alignment . y) (charSize ^. y `div` 3)
  & set (hitbox . size . x) (charSize ^. x `div` 3)
  & set (hitbox . size . y) (charSize ^. y `div` 3)


update :: Input -> MainChar -> Result (MainChar, DL.DList Bullet -> DL.DList Bullet)
update input mc = do
  wsize <- _windowSize <$> SM.get
  let
    dir = keysToMovement 1 input
    (mv, move) =
      MV.update dir
        . set MV.maxSpeed (if keyPressed KeyB input then Point 1.2 1.2 else Point 4 4)
        $ (mc ^. movement)

    addBullets
      | keyPressed KeyA input
      , mc ^. bulletsTimer == 0 =
        DL.append $ DL.fromList (newBullet mc)
      | otherwise = id

    newDir
      | keyPressed KeyLeft  input = DirLeft
      | keyPressed KeyRight input = DirRight
      | otherwise = mc ^. lastDir

    newMC =
      mc
      & over pos (`addPoint` move)
      & fixPos wsize
      & fixHitpos
      & set (size . x) (if keyPressed KeyB input then charSize ^. x `div` 2 else charSize ^. x)
      & (if keyPressed KeyB input then halfHitbox else id)
      & set movement mv
      & set lastDir newDir
      & over sprite
        (flip Spr.update False $
           if
             | keyPressed KeyB input && newDir == DirRight -> Just "side-left"
             | keyPressed KeyB input -> Just "side-right"
             | otherwise -> Just "normal"
        )
      & over hitTimer (\t -> if t <= 0 then -1 else t - 1)
      & over bulletsTimer (\t -> if t > 0 then t - 1 else if keyPressed KeyA input then 5 else 0)

    result =
      if mc ^. health <= 0 && mc ^. hitTimer < 0
        then (set size (Point 0 0) mc, id)
        else (newMC, addBullets)

  pure result

newBullet :: MainChar -> [Bullet]
newBullet mc
  | mc ^. size . x == charSize ^. x =
    [ mkBullet (mc ^. bullet) (Point 0 (-1)) mv 2 100 ((mc ^. pos) `addPoint` Point (mc ^. size . x `div` 4) 0)
    , mkBullet (mc ^. bullet) (Point 0 (-1)) mv 2 100 ((mc ^. pos) `addPoint` Point ((mc ^. size . x `div` 4) * 3) 0)
    ]
  | otherwise =
    [ mkBullet (mc ^. bullet) (Point 0 (-1)) mv 5 140 ((mc ^. pos) `addPoint` Point (charSize ^. x `div` 2) 0)
    ]

  where
    mv = MV.make $ MV.defArgs
      { MV.maxspeed = Point 0 10
      , MV.accel = Point 0 10
      }

checkHit :: [Bullet] -> MainChar -> MainChar
checkHit bullets mc
  | not (null bullets) && mc ^. health > 0
  = mc
    & over health (flip (-) (maximum $ (0:) $ map (^. damage) bullets))
    & \mc' -> set hitTimer (if mc' ^. health <= 0 then hitTimeout * 4 else hitTimeout) mc'
  | otherwise
  = mc

hitTimeout = 20

render :: SDL.Renderer -> Camera -> MainChar -> IO ()
render renderer cam mc =
  unless (mc ^. health < 0 && mc ^. hitTimer < 0) $ do
    let
      isHit = mc ^. hitTimer > 0 && mc ^. hitTimer `mod` 8 < 4
    Spr.render renderer cam (mc ^. pos) charSize (if isHit then 100 else 255) (mc ^. sprite)

get mc l
  | mc ^. health <= 0 && mc ^. hitTimer < 0 = Nothing
  | otherwise = pure $ mc ^. l
