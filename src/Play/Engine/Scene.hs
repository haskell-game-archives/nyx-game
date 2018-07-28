{-# LANGUAGE ExistentialQuantification #-}

module Play.Engine.Scene where

import Prelude hiding (head)
import qualified Play.Engine.MySDL.MySDL as MySDL
import SDL
import Play.Engine.Utils
import Play.Engine.Input
import Play.Engine.Settings
import Control.Monad.Except


-----------
-- Scene --
-----------

-- | Scene describes a generic interface for a game Scene
--   * state: for the scene data
--   * update: how to update the scene
--   * render: how to render the scene
data SceneF a
  = SceneF
  { scState :: a
  , scUpdate
      :: Input
      -> a
      -- it might fail with a reason
      -> Result ([MySDL.Request], (StackCommand, a))
  , scRender :: SDL.Renderer -> a -> IO ()
  }

-- | Existentially Quantified Scene
data Scene
  = forall s. Scene (SceneF s)

-- | Create a Scene from the interface
mkScene
  :: a
  -> (Input -> a -> Result (StackCommand, a))
  -> (SDL.Renderer -> a -> IO ())
  -> Scene
mkScene s u r = Scene (SceneF s (\i a -> fmap pure $ u i a) r)

{-
-- a sample definition of a state. Does nothing.
sample :: Scene
sample = mkScene
  ()
  (\_ () -> pure (None, ()))
  (const pure)
-}

-----------------
-- State stack --
-----------------

-- | A command for the scene stack, to be returned by `update`
data StackCommand
  = Done -- ^ You can remove me from the stack
  | None -- ^ Keep me at the top of the stack
  | Push Scene -- ^ Push a new scene
  | Replace Scene -- ^ Replace me with a new scene

-- | Update the top state on the stack
updateScenes :: Input -> Stack Scene -> Result ([MySDL.Request], Stack Scene)
updateScenes input scenes = do
  (reqs, (cmd, newState)) <- updateScene input (head scenes)
  (reqs,) <$> case cmd of
    Done -> case pop scenes of
      (_, Nothing) -> throwError ["Unexpected empty stack of states"]
      (_, Just rest) -> pure rest
    None -> pure $ replace newState scenes
    Replace otherState -> pure $ replace otherState scenes
    Push otherState -> pure $ push otherState (replace newState scenes)

-- | Update an existentially quantified Scene
updateScene :: Input -> Scene -> Result ([MySDL.Request], (StackCommand, Scene))
updateScene input = \case
  Scene s ->
    flip fmap ((scUpdate s) input (scState s)) $ \case
      (reqs, (cmd, newState)) -> (reqs, (cmd, Scene $ s { scState = newState }))

-- | Render the top scene on the stack
renderTopScene :: SDL.Renderer -> Stack Scene -> IO ()
renderTopScene sdlRenderer states = case head states of
  Scene s ->
    (scRender s) sdlRenderer (scState s)
