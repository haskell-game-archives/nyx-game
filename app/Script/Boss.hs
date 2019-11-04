{-# LANGUAGE OverloadedStrings #-}

module Script.Boss where

import qualified Play.Engine.MySDL.MySDL as MySDL
import qualified SDL.Mixer as Mix

import Script
import Play.Engine
import qualified Enemy.Fast as Fast
import qualified GameState as GS
import qualified TextBox as TB
import qualified Data.Map as M
import qualified Script.End as End
import Data.Maybe (isNothing)


boss :: Bool -> Int -> Scene
boss playMusic tryNum =
  GS.mkGameState $ Script
    wantedAssets
    (lScript playMusic tryNum)
    (boss False $ tryNum + 1)


wantedAssets :: [(String, MySDL.ResourceType FilePath)]
wantedAssets =
  Fast.wantedAssets
  ++ TB.wantedAssets
  ++ [ ("astral-avatar", MySDL.Texture "astral-avatar.png")
     , ("battle", MySDL.Music "battle.ogg")
     , ("nyx-avatar", MySDL.Texture "nyx-avatar.png")
     ]


lScript :: Bool -> Int -> MySDL.Resources -> Script
lScript playMusic tryNum MySDL.Resources{ MySDL.textures = ts, MySDL.fonts = fs, MySDL.music = ms } =
  
  [ goToLoc $ Point 380 800
  ] ++
  ( if
      | tryNum <= 0 ->
        [ LoadTextBox act{ stopTheWorld = True } $
          TB.make TB.Bottom 7
          "Phew..."
          (M.lookup "nyx-avatar" ts) (M.lookup "unispace" fs)
        , Wait act { stopTheWorld = True } 120
        , LoadTextBox act{ stopTheWorld = True } $
          TB.make TB.Top 3
          "Not bad Nyx!"
          (M.lookup "astral-avatar" ts) (M.lookup "unispace" fs)
        , LoadTextBox act{ stopTheWorld = True } $
          TB.make TB.Top 3
          "But did you really think it'll be that easy?"
          (M.lookup "astral-avatar" ts) (M.lookup "unispace" fs)
        , LoadTextBox act{ stopTheWorld = True } $
          TB.make TB.Bottom 5
          "Who are you...?\nHow do you know my name?"
          (M.lookup "nyx-avatar" ts) (M.lookup "unispace" fs)
        , LoadTextBox act{ stopTheWorld = True } $
          TB.make TB.Top 3
          "I think you have bigger things to worry about right now!"
          (M.lookup "astral-avatar" ts) (M.lookup "unispace" fs)
        , LoadTextBox act{ stopTheWorld = True } $
          TB.make TB.Top 4
          "Here I come!"
          (M.lookup "astral-avatar" ts) (M.lookup "unispace" fs)
        , LoadTextBox act{ stopTheWorld = True } $
          TB.make TB.Bottom 2
          "!!!"
          (M.lookup "nyx-avatar" ts) (M.lookup "unispace" fs)
        ]
      | tryNum < 4 ->
        [ LoadTextBox act{ stopTheWorld = True } $
          TB.make TB.Top 4
          "Did you really think you can beat me?"
          (M.lookup "astral-avatar" ts) (M.lookup "unispace" fs)
        ]
      | tryNum < 7 ->
        [ LoadTextBox act{ stopTheWorld = True } $
          TB.make TB.Top 4
          "Haven't you have enough yet?"
          (M.lookup "astral-avatar" ts) (M.lookup "unispace" fs)
        ]
      | tryNum < 10 ->
        [ LoadTextBox act{ stopTheWorld = True } $
          TB.make TB.Top 5
          "That persistence... Admirable..."
          (M.lookup "astral-avatar" ts) (M.lookup "unispace" fs)
        ]
      | tryNum < 15 ->
        [ LoadTextBox act{ stopTheWorld = True } $
          TB.make TB.Top 4
          "Or maybe you just don't know when to give up..."
          (M.lookup "astral-avatar" ts) (M.lookup "unispace" fs)
        ]
      | tryNum < 25 ->
        [ LoadTextBox act{ stopTheWorld = True } $
          TB.make TB.Top 6
          "You are never going to beat me."
          (M.lookup "astral-avatar" ts) (M.lookup "unispace" fs)
        ]
      | otherwise ->
        [ LoadTextBox act{ stopTheWorld = True } $
          TB.make TB.Top 9
          "..."
          (M.lookup "astral-avatar" ts) (M.lookup "unispace" fs)
        ]
  ) ++
  [ PlayMusic Mix.Forever ("battle", M.lookup "battle" ms)
  | playMusic
  ] ++
  -- Boss
  [ Spawn $ sequence [Fast.make (Point 350 (-100)) ts]
  , WaitUntil noAction (\nyx bullets -> isNothing nyx || null bullets)

  , If (const . isNothing)
    [ Wait noAction 90
    , FadeOut act{ command = Replace $ boss False (tryNum + 1) } 0
    , Wait noAction 30
    ]
  , StopMusic
  , Wait act{ stopTheWorld = False } 2
  , Wait act{ stopTheWorld = True } 150

  -- End
  , LoadTextBox act{ stopTheWorld = True } $
    TB.make TB.Top 6
    "It's not over Nyx!"
    Nothing (M.lookup "unispace" fs)

  , LoadTextBox act{ stopTheWorld = True } $
    TB.make TB.Top 7
    "This is just the beginning!!"
    Nothing (M.lookup "unispace" fs)

  , LoadTextBox act{ stopTheWorld = True } $
    TB.make TB.Top 10
    "J u S t . Y o U . W a I t . . ."
    Nothing (M.lookup "unispace" fs)

  , Wait act{ stopTheWorld = True } 60

  , Shake

  , Wait act{ stopTheWorld = False } 150

  , Wait act{ stopTheWorld = True } 150

  , LoadTextBox act{ stopTheWorld = True } $
    TB.make TB.Bottom 4
    "What was that thing..?"
    (M.lookup "nyx-avatar" ts) (M.lookup "unispace" fs)

  , LoadTextBox act{ stopTheWorld = True } $
    TB.make TB.Bottom 4
    "Why did she know my name..?"
    (M.lookup "nyx-avatar" ts) (M.lookup "unispace" fs)

  , Wait act{ stopTheWorld = True } 60

  , LoadTextBox act{ stopTheWorld = True } $
    TB.make TB.Bottom 4
    "I did not know that at the time,"
    Nothing (M.lookup "unispace" fs)

  , LoadTextBox act{ stopTheWorld = True } $
    TB.make TB.Bottom 4
    "But my fight was far from over..."
    Nothing (M.lookup "unispace" fs)

  , FadeOut act{ command = Replace $ End.end True } 0

  ]

