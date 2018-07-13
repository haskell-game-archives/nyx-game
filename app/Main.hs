{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}  -- One more extension.
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}  -- To derive Show
{-# LANGUAGE TypeOperators      #-}

module Main where

import Options.Generic

import Play.Engine.Runner

import Play.Engine.Types
import Play.Engine.Utils
import Play.Engine.Settings
import qualified StartScreen as S

main :: IO ()
main = do
  config <- unwrapRecord "Nyx Game"
  let
    h | unHelpful $ small config = 850
      | otherwise = 1000

  runGame (settings h) (S.make `Stack` [])

settings :: Int -> Settings
settings h = def
  { _windowSize = Point 800 h
  }

data Config w = Config
  { small :: Bool <?> "Start game on smaller resolution"
  }
  deriving (Generic)

instance ParseRecord (Config Wrapped)
deriving instance Show (Config Unwrapped)
