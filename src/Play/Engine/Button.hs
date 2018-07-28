{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}

module Play.Engine.Button where

import qualified SDL
import qualified SDL.Font as SDLF

import Play.Engine
import Control.Lens
import SDL.Vect (V4(..))
import qualified Data.Text as T


data Button
  = Button
  { _font :: SDLF.Font
  , _text :: T.Text
  , _pos :: {-# UNPACK #-} !IPoint
  , _size :: {-# UNPACK #-} !IPoint
  , _isClicked :: Bool
  }

makeFieldsNoPrefix ''Button

make :: IPoint -> Size -> SDLF.Font -> T.Text -> Button
make posi sz fnt txt = do
  Button
    { _font = fnt
    , _text = txt
    , _pos = posi
    , _size = sz
    , _isClicked = False
    }

update :: Input -> Button -> Result (Bool, Button)
update input btn =
  pure
    $ (keyClicked KeyStart input || keyClicked KeyA input,)
    $ btn
    & over isClicked (|| keyClicked KeyStart input || keyClicked KeyA input)

render :: SDL.Renderer -> Bool -> Button -> IO ()
render renderer marked btn
  | btn ^. size == Point 0 0 =
    pure ()
  | otherwise = do
  let
    rect = toRect (btn ^. pos) (btn ^. size)

  SDL.rendererDrawColor renderer SDL.$= V4 0 0 0 255
  SDL.fillRect renderer (Just rect)
  SDL.rendererDrawColor renderer SDL.$= V4 100 (if marked then 255 else 130) (if btn ^. isClicked then 255 else 180) 255
  SDL.drawRect renderer (Just rect)

  if T.length (btn ^. text) < 1
    then pure ()
    else do
      txt <- SDL.createTextureFromSurface renderer
        =<< SDLF.solid (btn ^. font) (V4 255 255 255 255) (btn ^. text)
      ti <- SDL.queryTexture txt
      let
        loc =
          Point
            (btn ^. pos . x + ((btn ^. size . x - fromIntegral (SDL.textureWidth  ti)) `div` 2))
            (btn ^. pos . y + ((btn ^. size . y - fromIntegral (SDL.textureHeight ti)) `div` 2))
      SDL.copy
        renderer
        txt
        Nothing
        (Just $ toRect
          loc
          (Point (fromIntegral $ SDL.textureWidth ti) (fromIntegral $ SDL.textureHeight ti))
        )
      SDL.destroyTexture txt
