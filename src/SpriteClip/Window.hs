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
import qualified Data.Set as S
import Control.Monad (liftM, forM_)
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
createSpriteCutoutsWindow :: Cairo.Surface -> Complex Double -> Complex Double -> IO (App.App AppState)
createSpriteCutoutsWindow sheet' (dx:+dy) (scx:+scy) = App.createWindowWithCanvasAndEvents size' 30 initial eventmap
  where
    initial  = AppState { _sheet=sheet', _mouse=0.0:+0.0, _active=Nothing, _pin=Nothing, _cutouts=[], _keys=S.empty }
    size'    = floor (dx*scx):+floor (dy*scy)
    eventmap = App.EventMap { App.ondraw      = Just $ ondraw,
                              App.onanimate   = Just   onanimate,

                              App.onkeydown = Just onkeydown,
                              App.onkeyup   = Just onkeyup,

                              App.onmousedown = Nothing, -- Just onmousedown,
                              App.onmouseup   = Nothing, --Just onmouseup,

                              App.onmousemotion = Just onmousemotion,
                              App.ondelete      = Nothing }


-- |
-- createSheetSelectionWindow :: [(String, Cairo.Surface)] -> Window
-- createSheetSelectionWindow = do
-- return ()


--------------------------------------------------------------------------------------------------------------------------------------------
-- |
-- TODO: Settings (?)
-- TODO: Rename (?)
-- TODO: Proper coordinate system handling
-- TODO: Extract, turn into stand-alone executable, move components into libraries
makeSpritesheetCutouts :: Cairo.Surface -> IO [BoundingBox Double]
makeSpritesheetCutouts sheet = do
  --
  putStrLn "Preparing your sprite sheet..."

  size <- imageSurfaceSize sheet
  app  <- createSpriteCutoutsWindow sheet size (1.0:+1.0)

  -- Icon
  -- dragSetIconName
  windowSetIconFromFile (App._window app) "C:/Users/Jonatan/Desktop/Haskell/projects/SpriteClip/assets/images/healthpotion.png"

  -- Cursor
  -- cursor <- cursorNew Pencil
  -- children <- containerGetChildren (App._window app)
  -- forM_ children (liftM (maybe (return ()) (flip drawWindowSetCursor (Just cursor))) . widgetGetWindow)
  -- forM_ children (liftM (maybe (return ()) (const $ putStrLn "Setting cursor")) . widgetGetWindow)
  -- forM_ children (const $ putStrLn "Child")

  --
  (App._canvas app) `on` buttonPressEvent   $ onmousedown (App._state app)
  (App._canvas app) `on` buttonReleaseEvent $ onmouseup   (App._state app)

  --
  (App._canvas app) `on` keyPressEvent $ onkeydown (App._state app)

  --
  (App._window app) `on` realize $ onrealised (app)

  -- Drag and drop
  widgetAcceptsDNDEvents (App._canvas app)
  widgetAddDNDListeners  app (App._canvas app)

  -- Launch
  App.run $ return app
  liftM (^. cutouts) $ readIORef (App._state app)


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
