-- |
-- Module      : SpriteClip.Events
-- Description :
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
--

-- Created September 7 2015

-- TODO | -
--        -

-- SPEC | -
--        -



-------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Pragmas
-------------------------------------------------------------------------------------------------------------------------------------------



-------------------------------------------------------------------------------------------------------------------------------------------
-- API
-------------------------------------------------------------------------------------------------------------------------------------------
 module SpriteClip.Events (module SpriteClip.Events) where



-------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
-------------------------------------------------------------------------------------------------------------------------------------------
import Data.Complex
import Data.IORef
import Control.Monad (liftM)

import qualified Graphics.Rendering.Cairo as Cairo --
import           Graphics.UI.Gtk          as Gtk   --

import qualified Southpaw.Cartesian.Plane.BoundingBox as BBox

import SpriteClip.Types
import SpriteClip.Lenses
import SpriteClip.Render as Render



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------
-- Rendering -------------------------------------------------------------------------------------------------------------------------------
-- |
ondraw :: Complex Double -> Cairo.Surface -> CutoutState -> Cairo.Render ()
ondraw canvasSize sheet state@(cutouts, mouse, mclick) = Render.render canvasSize sheet cutouts mclick mouse


-- Animation -------------------------------------------------------------------------------------------------------------------------------
-- |
onanimate :: Gtk.DrawingArea -> IORef CutoutState -> IO Bool
onanimate cvs cutouts = Gtk.widgetQueueDraw cvs >> return True


-- Keyboard --------------------------------------------------------------------------------------------------------------------------------
-- |
onkeypress :: IORef CutoutState -> EventM EKey Bool
onkeypress cutouts = do
  released <- liftM (Gtk.Release `elem`) Gtk.eventModifier
  if released
    then return False
    else return False
  return False


-- Mouse -----------------------------------------------------------------------------------------------------------------------------------
-- |
onmousemotion :: IORef CutoutState -> EventM EMotion Bool
onmousemotion cutouts = do
  (mx, my) <- eventCoordinates
  Cairo.liftIO $ modifyIORef cutouts (mouse' .~ (mx:+my))
  return False


-- |
onmousedown :: IORef CutoutState -> EventM EButton Bool
onmousedown cutouts = do
  -- TODO: Check which button it is
  -- mouse <- Cairo.liftIO $ readIORef cutouts
  (mx, my) <- eventCoordinates
  Cairo.liftIO $ modifyIORef cutouts (click' .~ Just (mx:+my))

  Cairo.liftIO $ putStrLn "Setting icon"
  w <- eventWindow
  cursor <- Cairo.liftIO $ Gtk.cursorNew Gtk.Hand1
  Cairo.liftIO $ Gtk.drawWindowSetCursor w (Just cursor)

  return True -- Keep sending motion-notify events


-- |
onmouseup :: IORef CutoutState -> EventM EButton Bool
onmouseup cutouts = do
  (mx, my) <- eventCoordinates
  -- TODO: Flip corners if necessary (no negative sizes)
  -- ((makebox topleft (cutouts ^. _2)) :)
  Cairo.liftIO $ modifyIORef cutouts (\s -> maybe s (\click -> cutouts' %~ (BBox.fromCorners click (mx:+my) :) $ s) (s ^. click'))

  s <- Cairo.liftIO $ readIORef cutouts

  Cairo.liftIO $ modifyIORef cutouts (click' .~ Nothing)

  return True


-- Drag'n'Drop -----------------------------------------------------------------------------------------------------------------------------
