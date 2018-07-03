{-# LANGUAGE OverloadedStrings #-}

module Script.Introduction where

import qualified Play.Engine.MySDL.MySDL as MySDL

import Script
import Play.Engine.Types
import qualified Data.Text as T
import qualified Data.Map as M
import qualified TextBox as TB
import qualified Play.Engine.Sprite as Spr
import qualified Play.Engine.State as State
import qualified Script.Level1 as L1
import qualified VN


intro :: State.State
intro =
  VN.make 1 $ Script
    wantedAssets
    introScript
    intro

wantedAssets :: [(String, MySDL.ResourceType FilePath)]
wantedAssets =
  TB.wantedAssets
  ++ [ ("music", MySDL.Music "shushushu.ogg")
     , ("nyx-avatar", MySDL.Texture "nyx-avatar.png")
     , ("customer-avatar", MySDL.Texture "customer.png")
     ]


introScript :: MySDL.Resources -> Script
introScript MySDL.Resources{ MySDL.textures = ts, MySDL.fonts = fs, MySDL.music = ms } =

  [ PlayMusic ("music", M.lookup "music" ms)

  , let
      sprargs rint =
        Spr.make $ Spr.MakeArgs
          { mkActionmap = ["normal"]
          , mkAction = "normal"
          , mkTexture = rint
          , mkSize = Point 800 1000
          , mkMaxPos = 4
          , mkSpeed = 30
          }
      spr = sprargs =<< M.lookup "bga" ts
    in
      Wait act{ changeSprite = spr } 0

  , Wait noAction 120
  , nyxInnerVoice fs "Hi."
  , nyxInnerVoice fs "I'm Nyx."

  , nyxInnerVoice fs "I work at this store. Fixing computers."
  , nyxInnerVoice fs $ T.unwords
    [ "While there still aren't many computers in personal use, their numbers keep increasing by the day."
    ]
  , nyxInnerVoice fs $ T.unwords
    [ "So I believe this is a good business to be in."
    ]
  , nyxInnerVoice fs "Sorry, gotta work now. A customer just came in."

  , customerVoice ts fs "Hey."
  , customerVoice ts fs "This is Nyx Fix, right? You fix computers?"

  , nyxVoice ts fs "Yeah.          \nHow can I help you?"

  , customerVoice ts fs "My computer is stuck. I can't run any programs. Can you do anything about it?"

  , nyxVoice ts fs "Let me have a look..."
  , Wait noAction 120

  , nyxVoice ts fs "Yep.        \nIt looks like it's infected with viruses."

  , customerVoice ts fs "A virus?! Will I get sick?"

  , nyxVoice ts fs "Computer viruses. It only affects computers."
  , nyxVoice ts fs "I can fix it but I'll need to keep it here for a few days."
  , nyxVoice ts fs "Can you leave your computer and your contact information?\nI'll call you when it's fixed."

  , customerVoice ts fs "Oh, good. Yes. Thank you. I'll wait for your call."

  , nyxVoice ts fs "Have a nice day."

  , customerVoice ts fs "You too."


  , StopMusic
  , Wait noAction 100

  , LoadTextBox noAction $
    TB.make TB.Top 15 "*Click*" Nothing (M.lookup "unispace" fs)
  , customerVoice ts fs "..."

  , customerVoice ts fs "Yeah, it's me."
  , customerVoice ts fs "It is done."
  , customerVoice ts fs "Now all we can do is wait and see if she can actually do it..."

  , Wait noAction 100
  , nyxVoice ts fs "Alright."
  , nyxVoice ts fs "Let the fixing begin!"


  , Wait noAction 60

  , Wait act{ command = State.Replace $ L1.level1 True } 60
  ]


nyxInnerVoice fs txt =
  LoadTextBox noAction $
    TB.make TB.Bottom 3 txt
    Nothing
    (M.lookup "unispace" fs)

nyxVoice ts fs txt =
  LoadTextBox noAction $
    TB.make TB.Bottom 3 txt
    (M.lookup "nyx-avatar" ts)
    (M.lookup "unispace" fs)

customerVoice ts fs txt =
  LoadTextBox noAction $
    TB.make TB.Top 3 txt
    (M.lookup "customer-avatar" ts)
    (M.lookup "unispace" fs)
