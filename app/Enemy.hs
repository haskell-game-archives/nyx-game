{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Enemy where

import qualified SDL

import Data.Maybe
import Play.Engine.Utils
import Play.Engine.Types
import Play.Engine.Input
import Play.Engine.Settings
import Control.Lens hiding (parts)
import Control.DeepSeq
import qualified Control.Monad.State as SM
import qualified Data.DList as DL

import Bullet
import qualified Attack as A
import qualified DecorationObject as DO
import qualified DecObj.SplitTexture as ST
import qualified Play.Engine.Movement as MV
import qualified Play.Engine.Sprite as Spr



data Enemy
  = Enemy
  { _pos :: {-# UNPACK #-} !IPoint
  , _size :: {-# UNPACK #-} !Size
  , _sprite :: {-# UNPACK #-} !Spr.Sprite
  , _hitbox :: {-# UNPACK #-} !Hitbox
  , _movement :: {-# UNPACK #-} !MV.Movement
  , _direction :: {-# UNPACK #-} !FPoint
  , _directionChanger :: Size -> Enemy -> FPoint
  , _attack :: {-# UNPACK #-} !A.Attack
  , _attackId :: {-# UNPACK #-} !Int
  , _attackChanger :: Enemy -> Int -> Maybe A.Attack
  , _health :: {-# UNPACK #-} !Int
  , _deathTime :: {-# UNPACK #-} !Int
  , _deathParts :: {-# UNPACK #-} !IPoint
  , _timers :: {-# UNPACK #-} !EnemyTimers
  }

data EnemyTimers
  = EnemyTimers
  { _hitTimer :: {-# UNPACK #-} !Int
  , _gracePeriodTimer :: {-# UNPACK #-} !Int
  }


instance NFData Enemy where
  rnf (Enemy {_pos, _size, _movement, _health, _timers}) =
    rnf _pos
    `seq` rnf _size
    `seq` rnf _movement
    `seq` rnf _timers
    `seq` rnf _health
    `seq` rnf _attack
    `seq` rnf _deathTime
    `seq` rnf _deathParts

instance NFData EnemyTimers where
  rnf (EnemyTimers {_hitTimer}) =
    rnf _hitTimer
    `seq` rnf _gracePeriodTimer


makeFieldsNoPrefix ''Enemy
makeFieldsNoPrefix ''EnemyTimers

instance Eq Enemy where
  e1 == e2 =
    e1 ^. pos == e2 ^. pos
    && e1 ^. size == e2 ^. size

instance Ord Enemy where
  e1 <= e2 =
    e1 ^. pos <= e2 ^. pos
    && e1 ^. size <= e2 ^. size


data MakeEnemy
  = MakeEnemy
  { mkePos :: {-# UNPACK #-} !IPoint
  , mkeSize :: {-# UNPACK #-} !Size
  , mkeSprite :: {-# UNPACK #-} !Spr.Sprite
  , mkeHitbox :: {-# UNPACK #-} !Hitbox
  , mkeMov :: {-# UNPACK #-} !MV.Movement
  , mkeHealth :: {-# UNPACK #-} !Int
  , mkeDirChanger :: Size -> Enemy -> FPoint
  , mkeAtk :: {-# UNPACK #-} !A.Attack
  , mkeAtkChanger :: Enemy -> Int -> Maybe A.Attack
  , mkeDeathTime :: {-# UNPACK #-} !Int
  , mkeDeathParts :: {-# UNPACK #-} !IPoint
  }

mkEnemy :: MakeEnemy -> Enemy
mkEnemy MakeEnemy{..} =
  Enemy
    { _pos = mkePos
    , _size = mkeSize
    , _sprite = mkeSprite
    , _hitbox = mkeHitbox
    , _direction = Point 0 0
    , _movement = mkeMov
    , _directionChanger = mkeDirChanger
    , _attack = mkeAtk
    , _attackId = 0
    , _attackChanger = mkeAtkChanger
    , _health = mkeHealth
    , _deathTime = mkeDeathTime
    , _deathParts = mkeDeathParts
    , _timers = initEnemyTimers
    }

initEnemyTimers :: EnemyTimers
initEnemyTimers = EnemyTimers
  { _hitTimer = -1
  , _gracePeriodTimer = 60 * 5
  }

update :: Input -> Enemy -> Result ([Enemy], DL.DList Bullet -> DL.DList Bullet, [DO.DecorationObject])
update _ enemy = do
  wsize <- _windowSize <$> SM.get
  let
    dir = (enemy ^. directionChanger) wsize enemy
    (mv, move) =
      MV.update dir
        $ (enemy ^. movement)

    (newBullets, attack') =
      (enemy ^. attack . A.attackUpdate) (enemy ^. pos) (enemy ^. size) (enemy ^. attack)

    changedAttack =
      (enemy ^. attackChanger) enemy (enemy ^. attackId)

    enemy' =
      enemy
      & over pos (if isAlive enemy then (`addPoint` move) else id)
      & over sprite (Spr.update Nothing False)
      & set movement mv
      & over timers updateTimers
      & set direction dir
      & set attack (fromMaybe attack' changedAttack)
      & over attackId (maybe id (const (+1)) changedAttack)

  parts <-
    if isDead enemy'
      then
        ST.make $ ST.MakeSplitTexture
          { mkPos = enemy' ^. pos
          , mkSize = enemy' ^. sprite . size
          , mkSplit = enemy' ^. deathParts
          , mkTexture = enemy' ^. sprite . Spr.texture
          , mkTexturePos = Point 0 0
          , mkDeathTime = enemy' ^. deathTime
          }
      else pure []

  pure
    ( if isDead enemy'
        || not (isInWindow wsize (enemy ^. pos) (enemy ^. size))
           && (enemy' ^. timers . gracePeriodTimer) < 0
        then []
        else pure enemy'
    , if isDead enemy'
        || not (isInWindow wsize (enemy ^. pos) (Point 0 0))
        then id
        else DL.append newBullets
    , parts
    )

updateTimers :: EnemyTimers -> EnemyTimers
updateTimers et =
  et
    & over hitTimer (\t -> if t <= 0 then -1 else t - 1)
    & over gracePeriodTimer (\t -> if t <= 0 then -1 else t - 1)


checkHit :: DL.DList Bullet -> Enemy -> Enemy
checkHit bullets enemy
  | any (isJust . flip isTouchingCircleRect enemy) bullets && enemy ^. health > 0
  = enemy
    & over health (flip (-) (DL.head bullets ^. damage))
    & \enemy' ->
      enemy'
        & set
           (timers . hitTimer)
           ( if
               | enemy' ^. health <= 0 ->
                 hitTimeout * 4
               | enemy' ^. timers . hitTimer < 0 ->
                 hitTimeout
               | otherwise ->
                 enemy' ^. timers . hitTimer
           )
  | otherwise
  = enemy

hitTimeout = 20

isDead :: Enemy -> Bool
isDead enemy =
  enemy ^. health <= 0
  && enemy ^. timers . hitTimer < 0

isAlive :: Enemy -> Bool
isAlive enemy =
  enemy ^. health > 0

render :: SDL.Renderer -> Camera -> Enemy -> IO ()
render renderer cam enemy = do
  let
    -- h = fromIntegral $ 255 - max 0 (enemy ^. health * 2)
    isHit = enemy ^. timers . hitTimer > 0 && enemy ^. timers . hitTimer `mod` 8 < 4

  Spr.render renderer cam (enemy ^. pos) (enemy ^. size) (if isHit then 100 else 255) (enemy ^. sprite)

  -- when isHit $ do
  --   let
  --     colour = Vect.V4 255 (255 - h) (255 - h) 150
  --     radius = fromIntegral $ enemy ^. size . x `div` 2
  --     center =
  --       Vect.V2
  --         (fromIntegral (cam (enemy ^. pos) ^. x) + radius)
  --         (fromIntegral (cam (enemy ^. pos) ^. y) + radius)
  --   SDL.circle renderer center radius colour
  --   SDL.fillCircle renderer center radius colour
