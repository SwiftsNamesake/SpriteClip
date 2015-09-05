-- |
-- Module      : Main
-- Description : 
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
-- 

-- Created September 5 2015

-- TODO | - 
--        - 

-- SPEC | -
--        -



---------------------------------------------------------------------------------------------------
-- API
---------------------------------------------------------------------------------------------------
module Main where



---------------------------------------------------------------------------------------------------
-- We'll need these
---------------------------------------------------------------------------------------------------
import Data.IORef
import Data.Complex
import Control.Monad (forM_, liftM)
import Control.Lens

import Text.Printf -- Debugging

import qualified Graphics.Rendering.Cairo as Cairo
import           Graphics.UI.Gtk          as Gtk

import qualified Southpaw.Interactive.Application as App
import qualified Southpaw.Picasso.Palette         as Palette
import           Southpaw.Picasso.RenderUtils

import           Southpaw.Cartesian.Plane
import           Triggers.Types

-- import qualified Triggers.Render as Render
import qualified Triggers.Render         as Render
import qualified Southpaw.Picasso.Render as Render



---------------------------------------------------------------------------------------------------
-- Functions
---------------------------------------------------------------------------------------------------
-- Sprite sheets ----------------------------------------------------------------------------------
-- |
type CutoutState = ([BoundingBox Double], Complex Double, Maybe (Complex Double))


-- cutouts' :: Field1 s t a b
cutouts' = _1

-- mouse' :: Field2 s t a b
mouse' = _2

-- click' :: Field3 s t a b
click' = _3


-- |
createSpriteCutoutsWindow :: Cairo.Surface -> IO (App.App CutoutState)
createSpriteCutoutsWindow sheet = App.createWindowWithCanvasAndEvents
                                    (size) -- Size (px:+px)
                                    (30)                                   -- Frame rate (Hz)
                                    ([BoundingBox (512:+512) (120:+316)], 0:+0, Nothing)                    -- App state
                                    (App.EventMap { App.ondraw        = Just ondraw,
                                                    App.onanimate     = Just onanimate,
                                                    App.onkeypress    = Just onkeypress,
        
                                                    App.onmousedown   = Nothing, -- Just onmousedown,
                                                    App.onmouseup     = Nothing, --Just onmouseup,
        
                                                    App.onmousemotion = Just onmousemotion,
                                                    App.ondelete = Nothing })
    where
      size = (floor $ 1024):+(floor $ 1024)

      ondraw (cutouts, mouse, mclick) = forM_ (cutouts ++ maybe [] (\click -> [makebox click mouse]) mclick) $ \cutout -> do

        -- Sprite sheet
        Render.image ((1024:+1024)*(0.5:+0)) (1024:+1024) sheet

        -- Outlines
        Render.rectangle (_centre cutout) (_size cutout)
        choose Palette.magenta
        Cairo.stroke

        -- Markers
        let (rectx:+recty) = _size cutout
            corners        = [(-1, 1), (1, 1), (1, -1), (-1, -1)]
            midline        = [(-1, 0), (1, 0), (0, -1), ( 0,  1)]
            centre         = [( 0, 0)]

        forM_ [(12, Palette.orange, corners), (10, Palette.peru, midline), (7, Palette.violetred, centre)] $ \ (radius, colour, offsets) ->
          forM_ offsets $ \(dx, dy) -> do
            choose colour
            Render.circle ((_centre cutout) + ((dx*rectx/2):+(dy*recty/2))) radius
            Cairo.fill

        -- Debug info (HUD)
        Cairo.moveTo 20 20
        Cairo.showText $ (printf "Mouse: X=%.02f, Y=%.02f" (realPart mouse) (imagPart mouse) :: String)

      onanimate cvs cutouts = Gtk.widgetQueueDraw cvs >> return True

      onkeypress cutouts    = do
        released <- liftM (Gtk.Release `elem`) Gtk.eventModifier
        if released
          then return False
          else return False
        return False
      
      onmousemotion :: IORef CutoutState -> EventM EMotion Bool
      onmousemotion cutouts = do
        (mx, my) <- eventCoordinates
        Cairo.liftIO $ modifyIORef cutouts (mouse' .~ (mx:+my))
        return False
      
      makebox topleft bottomright = let size = bottomright-topleft in BoundingBox { _centre=topleft+size*(0.5:+0.0), _size=size }
      
      -- tovector (x, y) = x:+y
      -- eventCoordinatesVector = liftM tovector eventCoordinates


-- |
-- TODO: Settings (?)
-- TODO: Proper coordinate system handling
-- TODO: Extract, turn into stand-alone executable, move components into libraries
makeSpritesheetCutouts :: Cairo.Surface -> IO [BoundingBox Double]
makeSpritesheetCutouts sheet = do
    app <- createSpriteCutoutsWindow sheet

    (App._canvas app) `on` buttonPressEvent   $ onmousedown (App._state app)
    (App._canvas app) `on` buttonReleaseEvent $ onmouseup   (App._state app)

    App.run $ return app
    liftM (^. _1) $ readIORef (App._state app)
    where
      onmousedown :: IORef CutoutState -> EventM EButton Bool
      onmousedown cutouts = do
        -- TODO: Check which button it is
        -- mouse <- Cairo.liftIO $ readIORef cutouts
        Cairo.liftIO $ putStrLn "DOWN"
        (mx, my) <- eventCoordinates
        Cairo.liftIO $ printf "Click at %f, %f\n" mx my
        Cairo.liftIO $ modifyIORef cutouts (click' .~ Just (mx:+my))
        return True -- Keep sending motion-notify events

      onmouseup :: IORef CutoutState -> EventM EButton Bool
      onmouseup cutouts = do
        Cairo.liftIO $ putStrLn "UP"
        (mx, my) <- eventCoordinates
        -- TODO: Flip corners if necessary (no negative sizes)
        -- ((makebox topleft (cutouts ^. _2)) :)
        Cairo.liftIO $ modifyIORef cutouts (\s -> maybe s (\click -> cutouts' %~ (makebox click (mx:+my) :) $ s) (s ^. _3))

        s <- Cairo.liftIO $ readIORef cutouts

        Cairo.liftIO $ printf "Topleft is %.02f, bottom right is %.02f\n" (mx) (my)
        Cairo.liftIO $ printf "Added %s\n" (show $ head $ view _1 s)

        Cairo.liftIO $ modifyIORef cutouts (click' .~ Nothing)

        return True
      
      makebox topleft bottomright = let size = bottomright-topleft in BoundingBox { _centre=topleft+size*(0.5:+0.0), _size=size }
