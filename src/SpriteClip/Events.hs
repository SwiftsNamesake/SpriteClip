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

-- TODO | - Encode all logic in types (state machine?)
--        - Preferences and keymaps
--        - Sophisticated command dispatch (eg. composite events, overlapping events)

-- SPEC | -
--        -



--------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Pragmas
--------------------------------------------------------------------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}



--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module SpriteClip.Events (module SpriteClip.Events) where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Data.Complex
import Data.Functor
import Data.Char (toLower)
import Data.IORef
import Data.Maybe (fromMaybe)

import qualified Data.Set  as S
import qualified Data.Text as T
import qualified Data.Map  as M

import Text.Printf
import Control.Monad (liftM, when, forM_, forM, unless)

import System.FilePath (takeExtension)

import qualified Graphics.Rendering.Cairo as Cairo --
import           Graphics.UI.Gtk          as Gtk   --

import qualified Southpaw.Interactive.Application     as App
import qualified Southpaw.Cartesian.Plane.BoundingBox as BBox
import Southpaw.Cartesian.Plane.Utilities

import SpriteClip.Types
import SpriteClip.Lenses
import SpriteClip.Render             as Render
import qualified SpriteClip.Commands as Commands



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------
-- Event configurations --------------------------------------------------------------------------------------------------------------------
-- |
-- widgetAcceptsDNDEvents ::
widgetAcceptsDNDEvents :: WidgetClass self => self -> IO ()
widgetAcceptsDNDEvents widget = do
    dragDestSet widget [DestDefaultMotion, DestDefaultDrop] [ActionCopy]
    -- dragDestAddTextTargets  widget
    -- dragDestAddImageTargets widget
    dragDestAddURITargets   widget


-- |
-- widgetAddDNDListeners ::
widgetAddDNDListeners :: WidgetClass widget => App.App AppState -> widget -> IO ()
widgetAddDNDListeners app widget = do
  widget `on` dragBegin        $ ondragbegin    app
  widget `on` dragMotion       $ ondragmotion   app -- TODO: Why won't this work?
  widget `on` dragDrop         $ ondragdrop     app
  widget `on` dragDataReceived $ ondragreceived app
  return ()


-- Rendering -------------------------------------------------------------------------------------------------------------------------------
-- |
-- TODO: Figure out canvasSize
ondraw :: AppState -> Cairo.Render ()
ondraw = Render.render


-- Animation -------------------------------------------------------------------------------------------------------------------------------
-- |
onanimate :: Gtk.DrawingArea -> IORef AppState -> IO Bool
onanimate cvs stateref = Gtk.widgetQueueDraw cvs >> return True


-- Keyboard --------------------------------------------------------------------------------------------------------------------------------
-- |
onkeydown :: IORef AppState -> EventM EKey Bool
onkeydown stateref = do
  key <- eventKeyName
  appstate <- Cairo.liftIO $ readIORef stateref

  Cairo.liftIO $ unless (S.member key $ appstate^.keys) $ do
    -- TODO: Needs heavy refactoring
    -- TODO: Use foldM (?)
    forM (Commands.dispatch (T.unpack key) (S.map T.unpack . S.insert key $ appstate^.keys) Commands.commands) $ \command -> do
      putStrLn "Running command"
      appstate <- readIORef stateref
      newstate <- command appstate
      writeIORef stateref newstate
      -- newstate <- maybe (return appstate) ($ appstate) $ M.lookup (S.fromList [key]) Commands.commands -- TODO: Factour out
    modifyIORef stateref (keys %~ S.insert (T.toLower key))

  --
  w <- eventWindow
  Cairo.liftIO $ do
    cursor <- cursorNew $ case key of
      "a" -> Pencil
      "b" -> Hand1
      _  -> BlankCursor

    -- children <- containerGetChildren w
    -- widgetGrabFocus
    -- forM_ [w] (liftM (maybe (return ()) (const $ putStrLn "Setting cursor")) . widgetGetWindow)
    forM_ [w] (flip drawWindowSetCursor (Just cursor))

  return True


-- |
onkeyup :: IORef AppState -> EventM EKey Bool
onkeyup stateref = do
  key <- eventKeyName
  Cairo.liftIO $ do
    modifyIORef stateref (keys %~ S.delete (T.toLower key))
    printf "Key %s was released.\n" (T.unpack key)
  return False


-- Mouse -----------------------------------------------------------------------------------------------------------------------------------
-- |
onmousemotion :: IORef AppState -> EventM EMotion Bool
onmousemotion stateref = do
  (mx, my) <- eventCoordinates
  Cairo.liftIO $ modifyIORef stateref (mouse .~ (mx:+my))
  return False


-- |
onmousedown :: IORef AppState -> EventM EButton Bool
onmousedown stateref = do
  -- TODO: Check which button it is
  -- mouse <- Cairo.liftIO $ readIORef cutouts
  (mx, my) <- eventCoordinates
  Cairo.liftIO $ modifyIORef stateref (pin .~ Just (mx:+my))

  -- Flesh out cursor behaviour (ie. grab hand when hover over marker)
  -- Cairo.liftIO $ putStrLn "Setting icon"
  -- w <- eventWindow
  -- cursor <- Cairo.liftIO $ Gtk.cursorNew Gtk.Hand1
  -- Cairo.liftIO $ Gtk.drawWindowSetCursor w Nothing -- (Just cursor)

  return True -- Keep sending motion-notify events


-- |
onmouseup :: IORef AppState -> EventM EButton Bool
onmouseup stateref = do
  (mx, my) <- eventCoordinates
  -- TODO: Flip corners if necessary (no negative sizes)
  -- ((makebox topleft (cutouts ^. _2)) :)
  Cairo.liftIO $ do
    modifyIORef stateref (\s -> maybe s (\click -> s & cutouts %~ (BBox.fromCorners click (mx:+my) :)) (s ^. pin))
    s <- readIORef stateref
    modifyIORef stateref (pin .~ Nothing)

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
ondragbegin :: App.App state -> DragContext -> IO ()
ondragbegin app context = do
  -- dragSetIconName
  Cairo.liftIO $ putStrLn "Drag begin"
  return ()


-- |
ondragstatus :: App.App state -> DragContext -> Maybe DragAction -> TimeStamp -> IO ()
ondragstatus app context maction timestamp = do
  Cairo.liftIO $ putStrLn "Drag status"
  return ()


-- |
-- ondragmotion ::
ondragmotion :: App.App AppState -> DragContext -> Point -> TimeStamp -> IO Bool
ondragmotion app context pt timestamp = do
  Cairo.liftIO $ putStrLn "Dragging..."
  atom <- atomNew ("String" :: String)
  s <- dragGetData (App._window app) context atom timestamp
  return True


-- |
-- ondragdrop ::
ondragdrop :: App.App state -> DragContext -> Point -> TimeStamp -> IO Bool
ondragdrop app context pt timestamp = do
  putStrLn "Dropping data. Catch!"
  return True


-- |
-- ondatareceived ::
ondragreceived :: App.App AppState -> DragContext -> Point -> InfoId -> TimeStamp -> SelectionDataM ()
ondragreceived app context pos infoid timestamp = do
  Cairo.liftIO $ putStrLn "Receing data..."
  s <- selectionDataGetURIs
  Cairo.liftIO $ print s
  Cairo.liftIO $ maybe
      (putStrLn "Didn't understand the drop")
      (\uris -> Cairo.liftIO $ when (takeExtension (uris !! 0) == ".png") $ do
         newsheet <- Cairo.imageSurfaceCreateFromPNG . drop (length ("file:///" :: String)) $ uris !! 0
         modifyIORef (App._state app) (sheet .~ newsheet)

         (cx:+cy) <- imageSurfaceSize newsheet >>= return . dotmap floor
         windowResize (App._window app) cx cy)
      -- (mapM_ (printf "Understood, here it is: <%s>.\n"))
      (s :: Maybe [String])


-- Size & Position -------------------------------------------------------------------------------------------------------------------------


-- Admin -----------------------------------------------------------------------------------------------------------------------------------
-- |
onrealised :: App.App AppState -> IO ()
onrealised app = do
  putStrLn "On realised"
