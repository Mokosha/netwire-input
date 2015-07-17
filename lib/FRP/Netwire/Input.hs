{-|
Module      : FRP.Netwire.Input
Description : Common wires to be used to support input in netwire
Copyright   : (c) Pavel Krajcevski, 2014
License     : MIT
Maintainer  : Krajcevski@gmail.com
Stability   : experimental
Portability : POSIX

This module contains definitions for typeclasses and wires to be used in FRP
programs that use netwire. In order to use this module, an implementation that
provides an instance of one of the underlying typeclasses 'MonadMouse' or
'MonadKeyboard' must be used. In order to not require the GLFW or SDL libraries
as dependencies, these instances are provided in separate libraries.

-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module FRP.Netwire.Input where

--------------------------------------------------------------------------------
-- Requried modules
import Prelude hiding (id, (.))
import Control.Monad.Fix
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Trans.Maybe
import Control.Monad.List
import Control.Monad.Trans.Identity
import Control.Monad.Cont
import Control.Wire
--------------------------------------------------------------------------------

-- | Turns a wire that outputs arbitrary events into one which puts the input
-- into the events, when they occur.
idifyEvent :: Monad m => Wire s e m a (Event b) -> Wire s e m a (Event a)
idifyEvent w = arr duplicate >>> first w >>> arr mkEvent
  where mkEvent (e, x) = fmap (const x) e
        duplicate x = (x,x)

--------------------------------------------------------------------------------
-- * Mouse input

-- | Mouse button typeclass. This is used to constrain the type of Monad used
-- | to provide mouse input.
class MouseButton a

-- | The mouse cursor mode. This mode is usually dependent on whether or not
-- the mouse is in the bounds of the application window.
data CursorMode = CursorMode'Disabled -- ^ The mouse cursor is disabled
                | CursorMode'Reset    -- ^ Reset the cursor to zero between computations
                | CursorMode'Hidden   -- ^ The mouse cursor is hidden when over the application
                | CursorMode'Enabled  -- ^ The mouse cursor is enabed and
                                      -- visible over the application
                deriving (Ord, Enum, Eq, Show, Read)

-- ** Mouse input

-- | This monad describes computations that involve mouse input.
class (MouseButton mb, Monad m) => MonadMouse mb m | m -> mb where
  -- | Sets the cursor mode for all subsequent computations. Note, that many
  -- | implementations require some sort of "poll" to read the IO
  setCursorMode :: CursorMode -> m ()
  -- | Returns true if the given mouse button is pressed
  mbIsPressed :: mb -> m Bool
  -- | Resets the pressed state of the mouse button
  releaseButton :: mb -> m ()
  -- | Get the current cursor location
  cursor :: m (Float, Float)
  -- | Get the amount of scrolling done in the x and y directions
  scroll :: m (Double, Double)

instance MonadMouse mb m => MonadMouse mb (StateT s m) where
  setCursorMode = lift . setCursorMode
  mbIsPressed = lift . mbIsPressed
  releaseButton = lift . releaseButton
  cursor = lift cursor
  scroll = lift scroll

instance MonadMouse mb m => MonadMouse mb (ReaderT r m) where
  setCursorMode = lift . setCursorMode
  mbIsPressed = lift . mbIsPressed
  releaseButton = lift . releaseButton
  cursor = lift cursor
  scroll = lift scroll

instance (Monoid w, MonadMouse mb m) => MonadMouse mb (WriterT w m) where
  setCursorMode = lift . setCursorMode
  mbIsPressed = lift . mbIsPressed
  releaseButton = lift . releaseButton
  cursor = lift cursor
  scroll = lift scroll

instance MonadMouse mb m => MonadMouse mb (ExceptT e m) where
  setCursorMode = lift . setCursorMode
  mbIsPressed = lift . mbIsPressed
  releaseButton = lift . releaseButton
  cursor = lift cursor
  scroll = lift scroll

instance MonadMouse mb m => MonadMouse mb (MaybeT m) where
  setCursorMode = lift . setCursorMode
  mbIsPressed = lift . mbIsPressed
  releaseButton = lift . releaseButton
  cursor = lift cursor
  scroll = lift scroll

instance MonadMouse mb m => MonadMouse mb (ListT m) where
  setCursorMode = lift . setCursorMode
  mbIsPressed = lift . mbIsPressed
  releaseButton = lift . releaseButton
  cursor = lift cursor
  scroll = lift scroll

instance MonadMouse mb m => MonadMouse mb (IdentityT m) where
  setCursorMode = lift . setCursorMode
  mbIsPressed = lift . mbIsPressed
  releaseButton = lift . releaseButton
  cursor = lift cursor
  scroll = lift scroll

instance MonadMouse mb m => MonadMouse mb (ContT r m) where
  setCursorMode = lift . setCursorMode
  mbIsPressed = lift . mbIsPressed
  releaseButton = lift . releaseButton
  cursor = lift cursor
  scroll = lift scroll

-- ** Mouse input wires

-- | Ignores its input and returns
-- the current normalized mouse coordinates. Regardless of window size,
-- each of the returned coordinates will be in the range @[-1, 1]@.
--
-- * Depends: now
-- * Inhibits: never
mouseCursor :: MonadMouse mb m => Wire s e m a (Float, Float)
mouseCursor = mkGen_ $ \_ -> liftM Right cursor

--------------------------------------------------------------------------------

-- | When the given mouse button is clicked (debounced).
mousePressed :: (MonadMouse mb m, Monoid e)
                   => mb -> Wire s e m a (Event a)
-- We do it this way instead of using the old-in netwire-input 'mouseDebounced'
-- because that achieves its function via 'releaseButton' which means that only
-- one instance of a 'mouseDebounced' can be valid for each 'MouseButton',
-- breaking all referential transparency. This way avoids that problem.
mousePressed mb = idifyEvent (became id . isMousePressed mb)

-- | When the mouse is released after being pressed.
mouseReleased :: (MonadMouse mb m, Monoid e)
                     => mb -> Wire s e m a (Event a)
mouseReleased mb = idifyEvent (noLonger id . isMousePressed mb)

-- | 'True' when the given mouse button is pressed, 'False' otherwise.
--
-- * Inhibits: never
isMousePressed :: (MonadMouse mb m, Monoid e)
                 => mb -> Wire s e m a Bool
isMousePressed mb = mkGen_ (const (Right <$> mbIsPressed mb))

-- | Returns the change in mouse coordinates between subsequent time instants
--
-- * Depends: before now
-- * Inhibits: never
mouseDelta :: (MonadFix m, MonadMouse mb m) => Wire s e m a (Float, Float)
mouseDelta = let
  delta (x, y) (x', y') = (x - x', y - y')
  in
   (mouseCursor >>>) . loop $ second (delay (0, 0)) >>>
   arr ( \(cur, lst) -> (delta cur lst, cur))

-- | The mouse mickies are the offset from zero at each time instant. If this
-- wire is being used, then it is assuming that the cursor mode is set to
-- 'CursorMode'Reset'
--
-- * Depends: now
-- * Inhibits: never
mouseMickies :: MonadMouse mb m => Wire s e m a (Float, Float)
mouseMickies =
  mkGen_ ( \a -> do { setCursorMode CursorMode'Reset; return (Right a) }) >>>
  mouseCursor

-- | The mouse scroll is the offset from zero at each time instant.
--
-- * Depends: now
-- * Inhibits: never
mouseScroll :: (Monoid e, MonadMouse mb m) => Wire s e m a (Double, Double)
mouseScroll = mkGen_ $ \_ -> liftM Right scroll

-- | The amount that the mouse has scrolled over the course of the entire wire.
--
-- * Depends: now
-- * Inhibits: never
mouseScrolled :: (Monoid e, MonadMouse mb m) => Wire s e m a (Double, Double)
mouseScrolled = mouseScroll >>> fn (0, 0)
  where
    fn (x, y) = mkSFN $ \(dx, dy) ->
      let result = (x + dx, y + dy)
      in (result, fn result)

-- | Behaves like the identity wire, but changes the cursor mode now.
--
-- * Inhibits: never
cursorMode :: (MonadMouse mb m, Monoid e) => CursorMode -> Wire s e m a a
cursorMode mode =
  mkGenN ( \x -> setCursorMode mode >> return (Right x, mkId))

--------------------------------------------------------------------------------
-- * Keyboard input

-- | Key typeclass. This is used to constrain the type of Monad used
-- | to provide keyboard input.
class Key a

-- | This monad describes computations that involve keyboard input.
class (Key k, Monad m) => MonadKeyboard k m | m -> k where
  -- | Returns true if the given key is currently pressed
  keyIsPressed :: k -> m Bool
  -- | Resets the pressed state of the given key.
  releaseKey :: k -> m ()

instance MonadKeyboard k m => MonadKeyboard k (StateT s m) where
  keyIsPressed = lift . keyIsPressed
  releaseKey = lift . releaseKey

instance MonadKeyboard k m => MonadKeyboard k (ReaderT r m) where
  keyIsPressed = lift . keyIsPressed
  releaseKey = lift . releaseKey

instance (Monoid w, MonadKeyboard k m) => MonadKeyboard k (WriterT w m) where
  keyIsPressed = lift . keyIsPressed
  releaseKey = lift . releaseKey

instance MonadKeyboard k m => MonadKeyboard k (ExceptT e m) where
  keyIsPressed = lift . keyIsPressed
  releaseKey = lift . releaseKey

instance MonadKeyboard k m => MonadKeyboard k (MaybeT m) where
  keyIsPressed = lift . keyIsPressed
  releaseKey = lift . releaseKey

instance MonadKeyboard k m => MonadKeyboard k (ListT m) where
  keyIsPressed = lift . keyIsPressed
  releaseKey = lift . releaseKey

instance MonadKeyboard k m => MonadKeyboard k (IdentityT m) where
  keyIsPressed = lift . keyIsPressed
  releaseKey = lift . releaseKey

instance MonadKeyboard k m => MonadKeyboard k (ContT r m) where
  keyIsPressed = lift . keyIsPressed
  releaseKey = lift . releaseKey

-- * Keyboard input wires

--------------------------------------------------------------------------------

-- | When the given key is pressed (debounced).
--
-- * Inhibits: never
keyPressed :: (MonadKeyboard k m, Monoid e)
                 => k -> Wire s e m a (Event a)
keyPressed k = idifyEvent (became id . isKeyPressed k)

-- | When the mouse is released after being pressed.
keyReleased :: (MonadKeyboard k m, Monoid e)
                     => k -> Wire s e m a (Event a)
keyReleased k = idifyEvent (noLonger id . isKeyPressed k)

-- | 'True' when the given key is pressed, 'False' otherwise.
isKeyPressed :: (MonadKeyboard k m, Monoid e)
              => k -> Wire s e m a Bool
isKeyPressed k = mkGen_ (const (Right <$> keyIsPressed k))
