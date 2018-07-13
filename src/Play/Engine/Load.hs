{-# LANGUAGE TemplateHaskell #-}

module Play.Engine.Load where

import SDL.Vect (V4(..))
import qualified SDL
import qualified Play.Engine.MySDL.MySDL as MySDL

import Play.Engine.Input
import Play.Engine.Settings
import qualified Play.Engine.State as State
import Control.Monad.Except
import Control.Lens

data State
  = State
  { _timer :: Int
  , _filepaths :: [(String, MySDL.ResourceType FilePath)]
  , _nextState :: MySDL.Resources -> Result State.State
  }

makeLenses ''State

mkState
  :: Int
  -> [(String, MySDL.ResourceType FilePath)]
  -> (MySDL.Resources -> Result State.State)
  -> State.State
mkState time files next =
  State.State $ State.StateF (initState time files next) update render

initState
  :: Int
  -> [(String, MySDL.ResourceType FilePath)]
  -> (MySDL.Resources -> Result State.State)
  -> State
initState = State

update :: Input -> State -> Result ([MySDL.Request], (State.Command, State))
update _ s@(State t _ _)
  | t > 0 = pure ([], (State.None, s & over timer (\x -> x - 1)))
update input s@(State _ files next) =
  case (files, responses input) of
    ([], []) -> pure ([], (State.None, s))
    ([], [MySDL.Exception e]) -> throwError [e]
    ([], [MySDL.ResourcesLoaded resources]) -> do
      next' <- next resources
      pure ([], (State.Replace next', s))
    ([], rs) -> throwError ["Unexpected number of responses: " ++ show (length rs)]
    (files', _) -> pure ([MySDL.Load files'], (State.None, set filepaths [] s))

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
