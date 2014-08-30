module FRP.Netwire.Input (
  Key, MouseButton,
  CursorMode(..),
  MonadKeyboard(..),
  keyPressed, keyDebounced,
  MonadMouse(..),
  mousePressed, mouseDebounced, mouseCursor, mouseCursorHidden, mouseMickies
) where

--------------------------------------------------------------------------------
import Control.Wire hiding ((.))
import Data.Monoid
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Mouse input

class MouseButton a

data CursorMode = CursorMode'Disabled
                | CursorMode'Hidden
                | CursorMode'Enabled
                deriving (Ord, Enum, Eq, Show, Read)

class (MouseButton mb, Monad m) => MonadMouse mb m | m -> mb where
  setCursorMode :: CursorMode -> m ()
  mbIsPressed :: mb -> m (Bool)
  releaseButton :: mb -> m ()
  cursor :: m (Float, Float)
  scroll :: m (Double, Double)

mouseCursor :: MonadMouse mb m => Wire s e m a (Float, Float)
mouseCursor = mkGen_ $ \_ -> cursor >>= (return . Right)

mouseCursorHidden :: MonadMouse mb m => Wire s e m a (Float, Float)
mouseCursorHidden = mkGen_ $ \_ -> do
  setCursorMode CursorMode'Hidden
  cursor >>= (return . Right)

mouseMickies :: MonadMouse mb m => Wire s e m a (Float, Float)
mouseMickies = mkGen_ $ \_ -> do
  setCursorMode CursorMode'Disabled
  cursor >>= (return . Right)

mousePressed :: (Monoid e, MonadMouse mb m) => mb -> Wire s e m a a
mousePressed mouse = mkGen_ $ \x -> do
  pressed <- mbIsPressed mouse
  if pressed
    then return (Right x)
    else return (Left mempty)

mouseDebounced :: (Monoid e, MonadMouse mb m) => mb -> Wire s e m a a
mouseDebounced mouse = mkGen_ $ \x -> do
  pressed <- mbIsPressed mouse
  if pressed
    then releaseButton mouse >> return (Right x)
    else return (Left mempty)

--------------------------------------------------------------------------------
-- Keyboard input

class Key a
class (Key k, Monad m) => MonadKeyboard k m | m -> k where
  keyIsPressed :: k -> m (Bool)
  releaseKey :: k -> m ()

keyPressed :: (Monoid e, MonadKeyboard k m) => k -> Wire s e m a a
keyPressed key = mkGen_ $ \x -> do
  pressed <- keyIsPressed key
  if pressed
    then return (Right x)
    else return (Left mempty)

keyDebounced :: (Monoid e, MonadKeyboard k m) => k -> Wire s e m a a
keyDebounced key = mkGen_ $ \x -> do
  pressed <- keyIsPressed key
  if pressed
    then releaseKey key >> return (Right x)
    else return (Left mempty)
