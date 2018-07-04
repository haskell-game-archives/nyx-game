{-# LANGUAGE OverloadedStrings #-}

module Script.End where

import qualified Play.Engine.MySDL.MySDL as MySDL

import Script
import qualified Play.Engine.State as State
import qualified GameState as GS
import qualified TextBox as TB
import qualified Data.Map as M


end :: Bool -> State.State
end playMusic =
  GS.mkGameState $ Script
    wantedAssets
    (lScript playMusic)
    (end False)


wantedAssets :: [(String, MySDL.ResourceType FilePath)]
wantedAssets =
  TB.wantedAssets
  ++ [ ("saito", MySDL.Texture "saito.png")
     , ("chikua", MySDL.Texture "chikua.png")
     , ("music-end", MySDL.Music "shushushu.ogg")
     , ("nyx-avatar", MySDL.Texture "nyx-avatar.png")
     ]


lScript :: Bool -> MySDL.Resources -> Script
lScript playMusic MySDL.Resources{ MySDL.textures = ts, MySDL.fonts = fs, MySDL.music = ms } =
  [ PlayMusic ("music-end", M.lookup "music-end" ms)
  | playMusic
  ] ++
  cycle
    (concat $ replicate 5
      [ LoadTextBox act{ stopTheWorld = True } $
        TB.make TB.Top 3 "Thanks for playing!" (M.lookup "saito" ts) (M.lookup "unispace" fs)
      , LoadTextBox act{ stopTheWorld = True } $
        TB.make TB.Bottom 3 "Thanks for playing!" (M.lookup "nyx-avatar" ts) (M.lookup "unispace" fs)
      ] ++ pure
      [ LoadTextBox act{ stopTheWorld = True } $
        TB.make TB.Top 5 "Thanks for playing!" (M.lookup "chikua" ts) (M.lookup "unispace" fs)
      , LoadTextBox act{ stopTheWorld = True } $
        TB.make TB.Bottom 3 "Thanks for playing!" (M.lookup "nyx-avatar" ts) (M.lookup "unispace" fs)
      ]
    )
