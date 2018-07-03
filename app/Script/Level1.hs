{-# LANGUAGE OverloadedStrings #-}

module Script.Level1 where

import qualified Play.Engine.MySDL.MySDL as MySDL

import Script
import Play.Engine.Types
import qualified Enemy.Static as St
import qualified Enemy.CrossDown as CDE
import qualified Enemy.SideToSideSpiral as SSE
import qualified Play.Engine.State as State
import qualified GameState as GS
import qualified Script.Level2 as L2
import qualified TextBox as TB
import qualified Data.Map as M


level1 :: Bool -> State.State
level1 playMusic =
  GS.mkGameState $ Script
    wantedAssets
    (lScript playMusic)
    (level1 False)


wantedAssets :: [(String, MySDL.ResourceType FilePath)]
wantedAssets =
  CDE.wantedAssets
  ++ St.wantedAssets
  ++ SSE.wantedAssets
  ++ TB.wantedAssets
  ++ [ ("saito",  MySDL.Texture "saito.png")
     , ("saito2", MySDL.Texture "saito2.png")
     , ("battle", MySDL.Music "battle.ogg")
     , ("nyx-avatar", MySDL.Texture "nyx-avatar.png")
     , ("customer-avatar", MySDL.Texture "customer.png")
     ]


lScript :: Bool -> MySDL.Resources -> Script
lScript playMusic MySDL.Resources{ MySDL.textures = ts, MySDL.fonts = fs, MySDL.music = ms } =
  
  -- [ Spawn $ sequence [Fast.make (Point 350 (-100)) ts]
  -- , WaitUntil noAction (const $ null)

  [ PlayMusic ("battle", M.lookup "battle" ms)
  | playMusic
  ] ++
  [ Wait noAction 60

  , LoadTextBox act{ stopTheWorld = True } $
    TB.make TB.Bottom 3 "Here they come." (M.lookup "nyx-avatar" ts) (M.lookup "unispace" fs)

  , Wait noAction 60
  ] ++
  -- First sequence
  concat
    ( replicate 3 $ concat
      [ spawnStaticAndWait 200 300 ts
      , spawnStaticAndWait 600 200 ts
      , spawnStaticAndWait 400 400 ts
      , spawnStaticAndWait 250 250 ts
      , spawnStaticAndWait 650 400 ts
      , spawnStaticAndWait 350 350 ts
      ]
    ) ++
  -- Mixing in up
  concat
    [ spawnStaticAndWait 200 300 ts
    , spawnStaticAndWait 600 200 ts
    , [Spawn $ sequence [CDE.make (Point 100 (-180)) (Right ()) ts]]
    , spawnStaticAndWait 400 400 ts
    , spawnStaticAndWait 250 250 ts
    , spawnStaticAndWait 650 400 ts
    , spawnStaticAndWait 350 350 ts
    ] ++
  -- Mixing in up
  concat
    [ spawnStaticAndWait 200 300 ts
    , spawnStaticAndWait 600 200 ts
    , spawnStaticAndWait 400 400 ts
    , spawnStaticAndWait 250 250 ts
    , [Spawn $ sequence [CDE.make (Point 700 (-180)) (Left ()) ts]]
    , spawnStaticAndWait 650 400 ts
    , spawnStaticAndWait 350 350 ts
    ] ++
  -- Mixing in up more
  concat
    [ spawnStaticAndWait 200 300 ts
    , [Spawn $ sequence [CDE.make (Point 100 (-180)) (Right ()) ts]]
    , spawnStaticAndWait 600 200 ts
    , spawnStaticAndWait 400 400 ts
    , [Spawn $ sequence [CDE.make (Point 700 (-180)) (Left ()) ts]]
    , spawnStaticAndWait 250 250 ts
    , spawnStaticAndWait 650 400 ts
    , spawnStaticAndWait 350 350 ts
    , [spawnTwoCDEs (Right ()) (Left ()) ts]
    ] ++

  -- First wave done
  [ WaitUntil noAction (const $ null)
  , Wait noAction 200
  , Wait act{ stopTheWorld = True } 30
  , Wait act{ command = State.Replace $ L2.level2 False } 60
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
