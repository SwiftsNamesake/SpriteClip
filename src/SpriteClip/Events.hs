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
import Text.Printf
import Control.Monad (liftM)

import qualified Graphics.Rendering.Cairo as Cairo --
import           Graphics.UI.Gtk          as Gtk   --

import qualified Southpaw.Interactive.Application     as App
import qualified Southpaw.Cartesian.Plane.BoundingBox as BBox

import SpriteClip.Types
import SpriteClip.Lenses
import SpriteClip.Render as Render



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------
-- Event configurations --------------------------------------------------------------------------------------------------------------------
-- |
-- widgetAcceptsDNDEvents ::
widgetAcceptsDNDEvents widget = do
    dragDestSet widget [DestDefaultMotion, DestDefaultDrop] [ActionCopy]
    dragDestAddTextTargets  widget
    dragDestAddImageTargets widget
    dragDestAddURITargets   widget


-- |
-- widgetAddDNDListeners ::
widgetAddDNDListeners :: WidgetClass widget => App.App stateref -> widget -> IO ()
widgetAddDNDListeners app widget = do
  widget `on` dragMotion       $ ondragmotion   app
  widget `on` dragBegin        $ ondragbegin    app
  widget `on` dragDrop         $ ondragdrop     app
  widget `on` dragDataReceived $ ondragreceived app
  return ()


-- Rendering -------------------------------------------------------------------------------------------------------------------------------
-- |
-- TODO: Figure out canvasSize
ondraw :: Complex Double -> AppState -> Cairo.Render ()
ondraw canvasSize appstate = Render.render canvasSize (appstate ^. sheet) (appstate^.cutouts) (appstate^.pin) (appstate^.mouse)


-- Animation -------------------------------------------------------------------------------------------------------------------------------
-- |
onanimate :: Gtk.DrawingArea -> IORef AppState -> IO Bool
onanimate cvs cutouts = Gtk.widgetQueueDraw cvs >> return True


-- Keyboard --------------------------------------------------------------------------------------------------------------------------------
-- |
onkeypress :: IORef AppState -> EventM EKey Bool
onkeypress cutouts = do
  released <- liftM (Gtk.Release `elem`) Gtk.eventModifier
  if released
    then return False
    else return False
  return False


-- Mouse -----------------------------------------------------------------------------------------------------------------------------------
-- |
onmousemotion :: IORef AppState -> EventM EMotion Bool
onmousemotion cutouts = do
  (mx, my) <- eventCoordinates
  Cairo.liftIO $ modifyIORef cutouts (mouse .~ (mx:+my))
  return False


-- |
onmousedown :: IORef AppState -> EventM EButton Bool
onmousedown stateref = do
  -- TODO: Check which button it is
  -- mouse <- Cairo.liftIO $ readIORef cutouts
  (mx, my) <- eventCoordinates
  Cairo.liftIO $ modifyIORef stateref (pin .~ Just (mx:+my))

  -- Flesh out cursor behaviour (ie. grab hand when hover over marker)
  Cairo.liftIO $ putStrLn "Setting icon"
  w <- eventWindow
  cursor <- Cairo.liftIO $ Gtk.cursorNew Gtk.Hand1
  Cairo.liftIO $ Gtk.drawWindowSetCursor w (Just cursor)

  return True -- Keep sending motion-notify events


-- |
onmouseup :: IORef AppState -> EventM EButton Bool
onmouseup stateref = do
  (mx, my) <- eventCoordinates
  -- TODO: Flip corners if necessary (no negative sizes)
  -- ((makebox topleft (cutouts ^. _2)) :)
  Cairo.liftIO $ modifyIORef stateref (\s -> maybe s (\click -> s & cutouts %~ (BBox.fromCorners click (mx:+my) :)) (s ^. pin))

  s <- Cairo.liftIO $ readIORef stateref

  Cairo.liftIO $ modifyIORef stateref (pin .~ Nothing)

  return True


-- Drag'n'Drop -----------------------------------------------------------------------------------------------------------------------------
-- |
acceptURIDrops :: IO ()
acceptURIDrops = do
  return ()


-- |
acceptImageDrops :: IO ()
acceptImageDrops = do
  return ()


-- |
-- ondragbegin ::
ondragbegin :: App.App stateref -> DragContext -> IO ()
ondragbegin app context = do
  Cairo.liftIO $ putStrLn "Drag begin"
  return ()


-- |
-- ondragmotion ::
ondragmotion :: App.App stateref -> DragContext -> Point -> TimeStamp -> IO Bool
ondragmotion app context pt timestamp = do
  Cairo.liftIO $ putStrLn "Dragging..."
  atom <- atomNew "String"
  s <- dragGetData (App._window app) context atom timestamp
  return True


-- |
-- ondragdrop ::
ondragdrop :: App.App stateref -> DragContext -> Point -> TimeStamp -> IO Bool
ondragdrop app context pt timestamp = do
  putStrLn "Dropping data. Catch!"
  return True


-- |
-- ondatareceived ::
ondragreceived :: App.App stateref -> DragContext -> Point -> InfoId -> TimeStamp -> SelectionDataM ()
ondragreceived app context pos infoid timestamp = do
  Cairo.liftIO $ putStrLn "Receing data..."
  s <- selectionDataGetURIs
  Cairo.liftIO $ maybe
      (putStrLn "didn't understand the drop")
      (mapM_ (printf "Understood, here it is: <%s>.\n"))
      (s :: Maybe [String])
