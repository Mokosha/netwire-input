module FRP.Netwire.Input (
  Key, MouseButton,
  CursorMode(..),
  MonadInput(..),
) where

--------------------------------------------------------------------------------
import Control.Wire
import Data.Monoid
--------------------------------------------------------------------------------

class Key a
class MouseButton a

data CursorMode = CursorMode'Disabled
                | CursorMode'Hidden
                | CursorMode'Enabled
                deriving (Ord, Enum, Eq, Show, Read)

class (Key k, MouseButton mb, Monad m) => MonadInput k mb m | m -> k, m -> mb where
  keyIsPressed :: k -> m (Bool)
  releaseKey :: k -> m ()
  mbIsPressed :: mb -> m (Bool)
  releaseButton :: mb -> m ()
  cursor :: m (Float, Float)
  setCursorMode :: CursorMode -> m ()
  scroll :: m (Double, Double)

--------------------------------------------------------------------------------
-- Wires

keyPressed :: (Monoid e, MonadInput k mb m) => k -> Wire s e m a a
keyPressed key = mkGen_ $ \x -> do
  pressed <- keyIsPressed key
  if pressed
    then return (Right x)
    else return (Left mempty)

keyDebounced :: (Monoid e, MonadInput k mb m) => k -> Wire s e m a a
keyDebounced key = mkGen_ $ \x -> do
  pressed <- keyIsPressed key
  if pressed
    then releaseKey key >> return (Right x)
    else return (Left mempty)

mousePressed :: (Monoid e, MonadInput k mb m) => mb -> Wire s e m a a
mousePressed mouse = mkGen_ $ \x -> do
  pressed <- mbIsPressed mouse
  if pressed
    then return (Right x)
    else return (Left mempty)

mouseDebounced :: (Monoid e, MonadInput k mb m) => mb -> Wire s e m a a
mouseDebounced mouse = mkGen_ $ \x -> do
  pressed <- mbIsPressed mouse
  if pressed
    then releaseButton mouse >> return (Right x)
    else return (Left mempty)
