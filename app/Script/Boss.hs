{-# LANGUAGE OverloadedStrings #-}

module Script.Boss where

import qualified Play.Engine.MySDL.MySDL as MySDL

import Script
import Play.Engine.Types
import qualified Enemy.CrossDown as CDE
import qualified Enemy.SideToSideSpiral as SSE
import qualified Enemy.Fast as Fast
import qualified Play.Engine.State as State
import qualified GameState as GS
import qualified TextBox as TB
import qualified Data.Map as M
import qualified Script.End as End


boss :: Bool -> Int -> State.State
boss playMusic tryNum =
  GS.mkGameState $ Script
    wantedAssets
    (lScript playMusic tryNum)
    (boss False $ tryNum + 1)


wantedAssets :: [(String, MySDL.ResourceType FilePath)]
wantedAssets =
  CDE.wantedAssets
  ++ SSE.wantedAssets
  ++ TB.wantedAssets
  ++ [ ("saito2", MySDL.Texture "saito2.png")
     , ("saito", MySDL.Texture "saito.png")
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
          (M.lookup "saito" ts) (M.lookup "unispace" fs)
        , LoadTextBox act{ stopTheWorld = True } $
          TB.make TB.Top 3
          "But did you really think it'll be that easy?"
          (M.lookup "saito" ts) (M.lookup "unispace" fs)
        , LoadTextBox act{ stopTheWorld = True } $
          TB.make TB.Bottom 5
          "Who are you...?\nHow do you know my name?"
          (M.lookup "nyx-avatar" ts) (M.lookup "unispace" fs)
        , LoadTextBox act{ stopTheWorld = True } $
          TB.make TB.Top 3
          "I think you have bigger things to worry about right now!"
          (M.lookup "saito" ts) (M.lookup "unispace" fs)
        , LoadTextBox act{ stopTheWorld = True } $
          TB.make TB.Top 4
          "Here I come!"
          (M.lookup "saito" ts) (M.lookup "unispace" fs)
        , LoadTextBox act{ stopTheWorld = True } $
          TB.make TB.Bottom 2
          "!!!"
          (M.lookup "nyx-avatar" ts) (M.lookup "unispace" fs)
        ]
      | tryNum < 4 ->
        [ LoadTextBox act{ stopTheWorld = True } $
          TB.make TB.Top 4
          "Did you really think you can beat me?"
          (M.lookup "saito" ts) (M.lookup "unispace" fs)
        ]
      | tryNum < 7 ->
        [ LoadTextBox act{ stopTheWorld = True } $
          TB.make TB.Top 4
          "Didn't you have enough yet?"
          (M.lookup "saito" ts) (M.lookup "unispace" fs)
        ]
      | tryNum < 10 ->
        [ LoadTextBox act{ stopTheWorld = True } $
          TB.make TB.Top 5
          "That persistence... Admirable..."
          (M.lookup "saito" ts) (M.lookup "unispace" fs)
        ]
      | tryNum < 15 ->
        [ LoadTextBox act{ stopTheWorld = True } $
          TB.make TB.Top 4
          "Or maybe you just don't know when to give up..."
          (M.lookup "saito" ts) (M.lookup "unispace" fs)
        ]
      | tryNum < 25 ->
        [ LoadTextBox act{ stopTheWorld = True } $
          TB.make TB.Top 6
          "You are never going to beat me."
          (M.lookup "saito" ts) (M.lookup "unispace" fs)
        ]
      | otherwise ->
        [ LoadTextBox act{ stopTheWorld = True } $
          TB.make TB.Top 9
          "..."
          (M.lookup "saito" ts) (M.lookup "unispace" fs)
        ]
  ) ++
  [ PlayMusic ("battle", M.lookup "battle" ms)
  | playMusic
  ] ++
  -- Boss
  [ Spawn $ sequence [Fast.make (Point 350 (-100)) ts]
  , WaitUntil noAction (const $ null)
  , Shake

  , Wait act{ stopTheWorld = False } 100
  , StopMusic

  , Wait act{ stopTheWorld = False } 150

  -- End
  , LoadTextBox act{ stopTheWorld = True } $
    TB.make TB.Top 8
    "It's not over Nyx!"
    Nothing (M.lookup "unispace" fs)

  , LoadTextBox act{ stopTheWorld = True } $
    TB.make TB.Top 9
    "This is just the beginning!!"
    Nothing (M.lookup "unispace" fs)

  , LoadTextBox act{ stopTheWorld = True } $
    TB.make TB.Top 15
    "J u S t . Y o U . W a I t . . ."
    Nothing (M.lookup "unispace" fs)

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

  , FadeOut 0

  , Wait act{ command = State.Replace $ End.end True } 60
  ]

