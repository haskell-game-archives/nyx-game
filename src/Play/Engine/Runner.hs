{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Play.Engine.Runner where

import qualified Data.Map as M
import Control.Monad.IO.Class (MonadIO)
import Control.Monad
import qualified SDL
import qualified Linear
import Control.Lens hiding (sets)
import Control.DeepSeq
import Control.Concurrent.STM.TQueue

import qualified Play.Engine.MySDL.MySDL as MySDL
import Play.Engine.Input
import Play.Engine.Types
import Play.Engine.Utils
import Play.Engine.Settings
import qualified Play.Engine.State as State

-----------
-- Logic --
-----------

runGame :: Settings -> Stack State.State -> IO ()
runGame sets w = do
  putStrLn "Hello Game!"
  _ <- run sets w
  putStrLn "Goodbye."

run :: Settings -> Stack State.State -> IO ()
run settings stack = do
  responsesQueue <- newTQueueIO
  resources <- MySDL.initResources
  void $ MySDL.withWindow "Game" (MySDL.myWindowConfig (Linear.V2 (winSize x) (winSize y))) $
    flip MySDL.withRenderer
      (\(window, ren) ->
          MySDL.apploop
            resources
            responsesQueue
            ren
            (settings, stack)
            update
            (render (window, ren) . snd)
      )
  where
    winSize l = fromIntegral $ settings ^. windowSize . l

update
  :: [MySDL.Response]
  -> [SDL.EventPayload]
  -> (SDL.Scancode -> Bool)
  -> (Settings, Stack State.State)
  -> IO (Either [String] ([MySDL.Request], (Settings, Stack State.State)))
update responses payload isKeyPressed (settings, stack) =
  let
    (keys, joykeys) = makeEvents (_keyStats settings) (_joyKeyStats settings) payload isKeyPressed (_keyMap settings)

    toggleMuteFlag
      | keyClicked' KeyM keys = not
      | otherwise = id

    settings' = settings
      & over muteMusic toggleMuteFlag
      & set keyStats keys
      & set joyKeyStats joykeys

    toggleMuteCmd
      | settings' ^. muteMusic = (:) MySDL.MuteMusic
      | not (settings' ^. muteMusic) = (:) MySDL.UnmuteMusic
      | otherwise = id

  in pure
    . fmap (\(setts, (reqs, states)) -> (toggleMuteCmd reqs, (setts, states)))
    . (joykeys `deepseq` keys `deepseq` runResult $! settings')
    $ State.updater (Input (M.unionWith max keys joykeys) responses) stack


render :: (SDL.Window, SDL.Renderer) -> Stack State.State -> IO ()
render (_, renderer) stack = do
  SDL.rendererDrawBlendMode renderer SDL.$= SDL.BlendAlphaBlend
  State.renderer renderer stack
  SDL.present renderer

setBGColorBlack :: MonadIO m => (SDL.Window, SDL.Renderer) -> m (SDL.Window, SDL.Renderer)
setBGColorBlack sdlStuff@(_, renderer) = do
  void $ MySDL.setBGColor (Linear.V4 0 0 0 255) renderer
  pure sdlStuff

