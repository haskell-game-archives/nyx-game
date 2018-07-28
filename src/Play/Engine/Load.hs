{-# LANGUAGE TemplateHaskell #-}

module Play.Engine.Load where

import SDL.Vect (V4(..))
import qualified SDL
import qualified Play.Engine.MySDL.MySDL as MySDL

import Play.Engine
import Control.Monad.Except
import Control.Lens

data State
  = State
  { _timer :: Int
  , _filepaths :: [(String, MySDL.ResourceType FilePath)]
  , _nextScene :: MySDL.Resources -> Result Scene
  }

makeLenses ''State

mkState
  :: Int
  -> [(String, MySDL.ResourceType FilePath)]
  -> (MySDL.Resources -> Result Scene)
  -> Scene
mkState time files next =
  Scene $ SceneF (initState time files next) update render

initState
  :: Int
  -> [(String, MySDL.ResourceType FilePath)]
  -> (MySDL.Resources -> Result Scene)
  -> State
initState = State

update :: Input -> State -> Result ([MySDL.Request], (StackCommand, State))
update _ s@(State t _ _)
  | t > 0 = pure ([], (None, s & over timer (\n -> n - 1)))
update input s@(State _ files next) =
  case (files, responses input) of
    ([], []) -> pure ([], (None, s))
    ([], [MySDL.Exception e]) -> throwError [e]
    ([], [MySDL.ResourcesLoaded resources]) -> do
      next' <- next resources
      pure ([], (Replace next', s))
    ([], rs) -> throwError ["Unexpected number of responses: " ++ show (length rs)]
    (files', _) -> pure ([MySDL.Load files'], (None, set filepaths [] s))

render :: SDL.Renderer -> State -> IO ()
render renderer state =
  let c = fromIntegral $ state ^. timer
  in void $ MySDL.setBGColor
    ( V4
      (20 `mod` 255)
      ((10 + (c `div` 3)) `mod` 255)
      ((20 + (c `div` 2)) `mod` 255)
      255
    )
    renderer
