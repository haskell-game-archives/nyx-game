{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Play.Engine.Runner where

import qualified Data.Map as M
import Control.Monad.IO.Class (MonadIO)
import Control.Monad
import qualified SDL
import SDL.Vect (V2(..), V4(..))
import Control.Lens hiding (sets)
import Control.DeepSeq
import Control.Concurrent.STM.TQueue

import qualified Play.Engine.MySDL.MySDL as MySDL
import Play.Engine.Input
import Play.Engine.Types
import Play.Engine.Utils
import Play.Engine.Settings
import Play.Engine.Scene

-----------
-- Logic --
-----------

runGame :: Settings -> Stack Scene -> IO ()
runGame sets w = do
  putStrLn "Hello Game!"
  _ <- run sets w
  putStrLn "Goodbye."

run :: Settings -> Stack Scene -> IO ()
run settings stack = do
  responsesQueue <- newTQueueIO
  resources <- MySDL.initResources
  void $ MySDL.withWindow "Game" (MySDL.myWindowConfig (V2 (winSize x) (winSize y))) $
    flip MySDL.withRenderer
      (\(window, ren) ->
          MySDL.apploop
            resources
            responsesQueue
            window
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
  -> (Settings, Stack Scene)
  -> IO (Either [String] ([MySDL.Request], (Settings, Stack Scene)))
update responses payload isKeyPressed (settings, stack) = do
  let
    (keys, joykeys) = makeEvents (_keyStats settings) (_joyKeyStats settings) payload isKeyPressed (_keyMap settings)

    toggleMuteFlag
      | keyClicked' KeyM keys = not
      | otherwise = id

    toggleWindowScale
      | keyClicked' KeyScale keys = \case
        NormalWindow -> SmallWindow
        SmallWindow -> NormalWindow
      | otherwise = id

    settings' = settings
      & over muteMusic toggleMuteFlag
      & over windowScale toggleWindowScale
      & set keyStats keys
      & set joyKeyStats joykeys

    toggleMuteCmd
      | settings' ^. muteMusic = (:) MySDL.MuteMusic
      | not (settings' ^. muteMusic) = (:) MySDL.UnmuteMusic
      | otherwise = id

    toggleWindowScaleCmd
      | keyClicked' KeyScale keys = case settings' ^. windowScale of
        NormalWindow -> (:) $ MySDL.SetNormalWindowScale $ settings' ^. windowSize
        SmallWindow -> (:) $ MySDL.SetSmallWindowScale $ settings' ^. windowSize
      | otherwise = id

  pure
    . fmap (\(setts, (reqs, states)) -> (toggleWindowScaleCmd $ toggleMuteCmd reqs, (setts, states)))
    . (joykeys `deepseq` keys `deepseq` runResult $! settings')
    $ updateScenes (Input (M.unionWith max keys joykeys) responses) stack


render :: (SDL.Window, SDL.Renderer) -> Stack Scene -> IO ()
render (_, renderer) stack = do
  SDL.rendererDrawBlendMode renderer SDL.$= SDL.BlendAlphaBlend
  renderTopScene renderer stack
  SDL.present renderer

setBGColorBlack :: MonadIO m => (SDL.Window, SDL.Renderer) -> m (SDL.Window, SDL.Renderer)
setBGColorBlack sdlStuff@(_, renderer) = do
  void $ MySDL.setBGColor (V4 0 0 0 255) renderer
  pure sdlStuff

