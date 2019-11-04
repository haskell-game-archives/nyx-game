module Script where

import Data.Word (Word8)
import Data.Maybe
import qualified SDL
import qualified SDL.Font as SDLF
import qualified SDL.Mixer as Mix
import qualified Play.Engine.MySDL.MySDL as MySDL
import qualified Data.ByteString as BS
import qualified Data.Text as T

import Enemy
import qualified TextBox as TB
import Control.Lens
import Control.Monad.Except
import Play.Engine
import qualified Play.Engine.Input as I
import qualified Play.Engine.Sprite as Spr


data Command
  = Wait !Actions Int
  | WaitUntil !Actions (Maybe IPoint -> [Enemy] -> Bool)
  | If (Maybe IPoint -> [Enemy] -> Bool) Script
  | Spawn (Result [Enemy])
  | LoadTextBox !Actions (Result TB.TextBox)
  | WaitTextBox !Actions TB.TextBox
  | PlayMusic Mix.Times (String, Maybe BS.ByteString)
  | PlayMusic' Mix.Times BS.ByteString
  | StopMusic
  | Shake
  | FadeOut !Actions Word8
  | FadeIn !Actions Word8
  | TextUp Word8 SDLF.Font IPoint T.Text

data ScriptData
  = Script
  { assets :: [(String, MySDL.ResourceType FilePath)]
  , script :: MySDL.Resources -> Script
  , restart :: Scene
  }

type Script = [Command]

data Actions
  = Actions
  { moveMC :: Maybe IPoint
  , spawn :: [Enemy]
  , stopTheWorld :: Bool
  , shake :: Bool
  , playMusic :: MusicAction
  , command :: StackCommand
  , changeSprite :: Maybe Spr.Sprite
  }

data MusicAction
  = MAPlay (String, FilePath)
  | MAContinue
  | MAStop

noAction :: Actions
noAction = Actions
  { moveMC = Nothing
  , spawn = []
  , stopTheWorld = False
  , playMusic = MAContinue
  , shake = False
  , command = None
  , changeSprite = Nothing
  }

act :: Actions
act = noAction

update :: I.Input -> Maybe IPoint -> [Enemy] -> Script -> Result (Actions, Script)
update input mcPos enemies = \case
  [] -> pure (noAction, [])
  Wait acts i : rest
    | i <= 0 -> pure (acts, rest)
    | otherwise -> pure (acts, Wait acts (i-1) : rest)

  WaitUntil acts test : rest
    | test mcPos enemies -> pure (acts, rest)
    | otherwise -> pure (acts, WaitUntil acts test : rest)

  If test cmds : rest
    | test mcPos enemies -> pure (noAction, cmds <> rest)
    | otherwise -> pure (noAction, rest)

  Spawn spawned : rest ->
    (, rest) . (\s -> noAction { spawn = s }) <$> spawned

  LoadTextBox acts rtb : rest -> do
    tb <- rtb
    pure (acts, WaitTextBox (acts { changeSprite = Nothing }) tb : rest)

  WaitTextBox acts tb : rest -> do
    TB.update input tb >>= \case
      Nothing -> pure  (acts, rest)
      Just tb' -> pure (acts, WaitTextBox acts tb' : rest)

  PlayMusic times (name, m) : rest -> do
    case m of
      Nothing ->
        throwError ["Audio asset not loaded: " ++ name]
      Just msc ->
        pure (noAction, PlayMusic' times msc : rest)

  PlayMusic' _ _ : rest ->
    pure (noAction, rest) -- `render` takes care of playing the music

  StopMusic : rest ->
    pure (noAction, rest) -- `render` takes care of playing the music

  Shake : rest ->
    pure (noAction { shake = True }, rest)

  FadeOut actions n : rest ->
    let
      jump = 2
    in case n of
      255 ->
        pure (actions, rest)
      _ | n > 255 - jump ->
        pure (noAction, FadeOut actions 255 : rest)
      _ ->
        pure (noAction, FadeOut actions (n + jump) : rest)

  FadeIn actions n : rest ->
    let
      jump = 2
    in case n of
      0 -> pure (actions, rest)
      _ | n < jump ->
        pure (noAction, FadeIn actions 0 : rest)
      _ ->
        pure (noAction, FadeIn actions (n - jump) : rest)

  TextUp spd font location txt : rest ->
    case location ^. y of
      n | n < -50 ->
        pure (noAction, rest)
      n ->
        pure (noAction, TextUp spd font (set y (n - fromIntegral spd) location) txt : rest)




goToLoc :: IPoint -> Command
goToLoc p =
  WaitUntil
    (act { moveMC = Just p })
    (\mcPos _ -> case mcPos of
        Nothing -> False
        Just posi -> isAround p posi (Point 20 20)
    )

render :: SDL.Renderer -> Camera -> Script -> IO ()
render renderer cam =
  maybe (pure ()) f . listToMaybe
  where
    f = \case
      WaitTextBox _ tb ->
        TB.render renderer tb

      PlayMusic' times m ->
        Mix.playMusic times =<< Mix.decode m

      StopMusic ->
        void $ Mix.fadeOutMusic (1000 * 2) -- milliseconds

      FadeOut _ n -> shade renderer cam n
      FadeIn  _ n -> shade renderer cam n

      TextUp _ font location txt ->
        renderText renderer font location txt

      _ -> pure ()

getNewText :: [MySDL.Response] -> Maybe MySDL.Response
getNewText = \case
  [] -> Nothing
  MySDL.NewText txt : _ ->
    pure $ MySDL.NewText txt
  MySDL.Exception e : _ -> pure $ MySDL.Exception e
  _ : r -> getNewText r
