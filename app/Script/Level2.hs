{-# LANGUAGE OverloadedStrings #-}

module Script.Level2 where

import qualified Play.Engine.MySDL.MySDL as MySDL

import Script
import Play.Engine.Types
import qualified Enemy.Static as St
import qualified Enemy.CrossDown as CDE
import qualified Enemy.SideToSideSpiral as SSE
import qualified Play.Engine.State as State
import qualified GameState as GS
import qualified Script.Boss as Boss
import qualified TextBox as TB
import qualified Data.Map as M


level2 :: Bool -> State.State
level2 playMusic =
  GS.mkGameState $ Script
    wantedAssets
    (lScript playMusic)
    (level2 False)


wantedAssets :: [(String, MySDL.ResourceType FilePath)]
wantedAssets =
  CDE.wantedAssets
  ++ SSE.wantedAssets
  ++ St.wantedAssets
  ++ TB.wantedAssets
  ++ [ ("saito",  MySDL.Texture "saito.png")
     , ("saito2", MySDL.Texture "saito2.png")
     , ("battle", MySDL.Music "battle.ogg")
     , ("nyx-avatar", MySDL.Texture "nyx-avatar.png")
     ]


lScript :: Bool -> MySDL.Resources -> Script
lScript playMusic MySDL.Resources{ MySDL.textures = ts, MySDL.fonts = fs, MySDL.music = ms } =
  
  [ goToLoc $ Point 380 800
  ] ++
  [ PlayMusic ("battle", M.lookup "battle" ms)
  | playMusic
  ] ++
  [ LoadTextBox act{ stopTheWorld = True } $
    TB.make TB.Bottom 4 "This is harder than I expected..." (M.lookup "nyx-avatar" ts) (M.lookup "unispace" fs)
  ] ++

  -- Second wave
  concat
    [ [ Spawn $ sequence [CDE.make (Point 100 (-180)) (Right ()) ts]
      , Wait noAction 100
      ]
    , spawnStaticAndWait 400 300 ts
    , [ Spawn $ sequence [CDE.make (Point 700 (-180)) (Left ()) ts]
      , Wait noAction 100
      ]
    , [ Spawn $ sequence [CDE.make (Point 650 (-150)) (Left ()) ts]
      , Wait noAction 100
      ]
    , [ Spawn $ sequence [CDE.make (Point 100 (-180)) (Right ()) ts]
      , Wait noAction 100
      ]
    , spawnStaticAndWait 600 300 ts
    , spawnStaticAndWait 250 250 ts
    , [ spawnTwoCDEs (Left ()) (Right ()) ts
      , Wait noAction 100
      , spawnTwoCDEs (Right ()) (Left ()) ts
      , Wait noAction 100
      ]
    , [ spawnTwoCDEs (Left ()) (Right ()) ts
      , Wait noAction 60
      , spawnTwoCDEs (Right ()) (Left ()) ts
      ]
    , spawnStaticAndWait 600 300 ts
    , spawnStaticAndWait 250 250 ts
    , [ spawnTwoCDEs (Left ()) (Right ()) ts
      , Wait noAction 60
      , spawnTwoCDEs (Right ()) (Left ()) ts
      ]
    ] ++

  -- Second wave done
  [ WaitUntil noAction (const $ null)
  , Wait noAction 200
  , Wait act{ stopTheWorld = True } 30
  , Wait act{ command = State.Replace $ Boss.boss False 0 } 60
  ]

spawnTwoCDEs dir1 dir2 ts =
  Spawn $ sequence
    [ CDE.make (Point 300 (-180)) dir1 ts
    , CDE.make (Point 400 (-180)) dir2 ts
    ]

spawnStaticAndWait posx target ts =
  [ Spawn $ sequence
    [ St.make (Point posx (-100)) (Point 0 1) target ts
    ]
  , Wait noAction 120
  ]
