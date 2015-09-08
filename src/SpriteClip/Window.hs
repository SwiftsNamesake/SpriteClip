-- |
-- Module      : SpriteClip.Window
-- Description :
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
--

-- Created September 8 2015

-- TODO | -
--        -

-- SPEC | -
--        -



--------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Pragmas
--------------------------------------------------------------------------------------------------------------------------------------------



--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module SpriteClip.Window where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We\'ll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Data.IORef
import Control.Monad (liftM)
import Text.Printf

import qualified Graphics.Rendering.Cairo as Cairo
import           Graphics.UI.Gtk

import qualified Southpaw.Interactive.Application as App
import           Southpaw.Picasso.RenderUtils
import           Southpaw.Interactive.Console

import SpriteClip.Types
import SpriteClip.Lenses
import SpriteClip.Events



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------
-- |
-- TODO: Take scaling into account (w.r.t image size and coordinate conversions)
createSpriteCutoutsWindow :: Cairo.Surface -> Complex Double -> Complex Double -> IO (App.App CutoutState)
createSpriteCutoutsWindow sheet (dx:+dy) (scx:+scy) = App.createWindowWithCanvasAndEvents size' 30 initial eventmap
  where
    initial  = ([], 0.0:+0.0, Nothing)
    size'    = floor (dx*scx):+floor (dy*scy)
    eventmap = App.EventMap { App.ondraw      = Just $ ondraw (dx:+dy) sheet,
                              App.onanimate   = Just   onanimate,
                              App.onkeypress  = Just   onkeypress,

                              App.onmousedown = Nothing, -- Just onmousedown,
                              App.onmouseup   = Nothing, --Just onmouseup,

                              App.onmousemotion = Just onmousemotion,
                              App.ondelete      = Nothing }


-----------------------------------------------------------------------------------------------------------------------
-- |
-- TODO: Settings (?)
-- TODO: Proper coordinate system handling
-- TODO: Extract, turn into stand-alone executable, move components into libraries
makeSpritesheetCutouts :: Cairo.Surface -> IO [BoundingBox Double]
makeSpritesheetCutouts sheet = do
  --
  putStrLn "Preparing your sprite sheet..."

  size <- imageSurfaceSize sheet
  app  <- createSpriteCutoutsWindow sheet size (1.0:+1.0)

  --
  (App._canvas app) `on` buttonPressEvent   $ onmousedown (App._state app)
  (App._canvas app) `on` buttonReleaseEvent $ onmouseup   (App._state app)

  -- Drag and drop
  widgetAcceptsDNDEvents (App._canvas app)
  widgetAddDNDListeners  app (App._canvas app)

  -- Launch
  App.run $ return app
  liftM (^. cutouts') $ readIORef (App._state app)


-- |
spriteClipMain :: IO ()
spriteClipMain = do
  imagepaths <- chooseFilesFromDirectory imagefolder (== ".png")
  putStrLn "Ok, you've chosen a sprite sheet"
  either
    (const $ putStrLn "That's not a sprite sheet.")
    (\paths -> Cairo.imageSurfaceCreateFromPNG (paths !! 0) >>= makeSpritesheetCutouts >>= print)
    (imagepaths)
  where
    imagefolder = "C:/Users/Jonatan/Desktop/Java/QMUL/Jon/2015/Feud/assets/"


-- | Drag'n'Drop test
demoDND :: IO ()
demoDND = do
    initGUI
    w <- windowNew
    l <- labelNew $ Just "drag here lol"

    w `on` destroyEvent $ Cairo.liftIO (mainQuit >> return False)
    w `on` deleteEvent  $ Cairo.liftIO (mainQuit >> return False)

    containerAdd w l

    widgetAddEvents l [PointerMotionMask]

    dragDestSet l [DestDefaultMotion, DestDefaultDrop] [ActionCopy]
    dragDestAddTextTargets l
    dragDestAddImageTargets l
    dragDestAddURITargets l

    -- l `on` dragBegin $ \ context -> do
    --   Cairo.liftIO $ putStrLn "Beginning drag..."
    --   return ()

    l `on` motionNotifyEvent $ Cairo.liftIO (putStrLn "Wheeee!" >> return True)

    l `on` dragMotion  $ \ctx p tm -> do
      Cairo.liftIO $ putStrLn "Dragging..."
      atom <- atomNew "String"
      s <- dragGetData l ctx atom tm
      return True

    l `on` dragDrop $ \ctx p tm -> do
      Cairo.liftIO $ putStrLn "Dropping data. Catch!"
      return True

    l `on` dragDataReceived $ \dc pos id ts -> do
      Cairo.liftIO $ putStrLn "Receing data..."
      s <- selectionDataGetURIs
      Cairo.liftIO $ maybe
          (putStrLn "didn't understand the drop")
          (mapM_ (printf "Understood, here it is: <%s>.\n"))
          (s :: Maybe [String])

      s <- selectionDataGetPixbuf
      -- maybe
      --   (_)
      --   (_)
      --   (s)
      return ()
    widgetShowAll w
    mainGUI
