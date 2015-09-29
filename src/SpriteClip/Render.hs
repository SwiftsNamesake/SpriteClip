-- |
-- Module      : SpriteClip.Render
-- Description :
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
--

-- Created September 7 2015

-- TODO | - UI and graphics settings
--        - Factor out logic (and settings, input mode, etc.) (should not be embedded in rendering functions)

-- SPEC | -
--        -



--------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Pragmas
--------------------------------------------------------------------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}



--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module SpriteClip.Render (module SpriteClip.Render,
                          module Render,
                          module Palette,
                          module Southpaw.Picasso.RenderUtils,
                          module Cairo) where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Data.Complex
import Data.Maybe
import qualified Data.Set as S

import Text.Printf
import Control.Monad (forM_, when, liftM)
import Control.Lens

import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.UI.Gtk          as Gtk

import           Southpaw.Picasso.Palette (alphapart, redpart, greenpart, bluepart)
import qualified Southpaw.Picasso.Palette     as Palette
import qualified Southpaw.Picasso.Render      as Render
import           Southpaw.Picasso.RenderUtils hiding (grid, closePath)

import qualified Southpaw.Cartesian.Plane.BoundingBox as BBox
import           Southpaw.Cartesian.Plane.Utilities

import           SpriteClip.Types
import           SpriteClip.Lenses
import qualified SpriteClip.Logic  as Logic



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------
-- | Renders a frame
render :: AppState -> Cairo.Render ()
render appstate = do
  --
  canvasSize <- imageSurfaceSize sheet'

  -- Render.imageWithClip (const $ const $ Render.circle (appstate ^. mouse) 120) (50:+50) (appstate ^. sheet)
  Render.image (100:+100) (appstate ^. sheet)

  --
  -- Render.image (canvasSize*0.5) (appstate ^. sheet) -- Render sprite sheet
  -- Cairo.liftIO $ print (length $ take 5 sections)
  forM_ sections' (section mouse') -- Render sprite sections with markers
  alignments sections' mouse'      -- Render alignments
  axes mouse' canvasSize           -- Render cursor axes
  debugHUD appstate                -- Render debug overlay
  where
    sections' = (appstate ^. cutouts) ++ maybe [] (\click -> [BBox.fromCorners click mouse']) (appstate ^. pin)                -- TODO: Refactor
    mouse'    = Logic.applyOptionals [(gridsnap, dotmap $ Logic.nearestMultiple 20.0), (square, smallest)] (appstate ^. mouse) -- TODO: Rename (eg. cursor or pointer)
    sheet'    = appstate ^. sheet
    gridsnap  = S.member "shift_r"    $ appstate ^. keys --
    square    = S.member "control_r"  $ appstate ^. keys --
    smallest (re:+im) = min re im :+ min re im           --


-- |
-- TODO: Options
section ::  Complex Double -> BoundingBox Double -> Cairo.Render ()
section mouse bbox = do
  -- Outline
  Render.rectangle (_centre bbox) (_size bbox)
  choose Palette.magenta
  Cairo.stroke

  -- Markers
  sectionMarkers bbox mouse


-- | Renders the markers for a sheet section (respresented as a 'BoundingBox')
-- TODO: Options
-- TODO: Move out hover logic (?)
-- TODO: Refactor
sectionMarkers :: BoundingBox Double -> Complex Double -> Cairo.Render ()
sectionMarkers bbox mouse = forM_ markerBunches $ \ (radius, colour, offsets) ->
    forM_ offsets $ \pt -> do
      choose colour
      Render.circle (centre pt) (r radius pt)
      Cairo.fill
  where
    scale     = max 0.3 $ min 1.6 (let szx:+szy = _size bbox in min (abs szx) (abs szy)/120.0) -- Scale depending on section size
    focused r = Logic.within (scale*r) mouse . centre                                          -- Does the cursor lie on the marker?
    r r' pt   = scale*r'*(if focused r' pt then 1.4 else 1.0)                                  --
    (rectx:+recty)  = _size bbox
    centre (dx:+dy) = _centre bbox + ((dx*rectx/2):+(dy*recty/2))
    markerBunches   = [(12, Palette.orange & alphapart .~ 0.88, Logic.corners),
                       (10, Palette.peru   & alphapart .~ 0.88, Logic.midline),
                       ( 7, Palette.plum   & alphapart .~ 0.88, Logic.centre)]


-- | Renders the alignments for a sprite section (respresented as a 'BoundingBox')
-- TODO: Don't assume the point represents the mouse
alignments :: [BoundingBox Double] -> Complex Double -> Cairo.Render ()
alignments bboxes mouse@(mx:+my) = do

  -- TODO: Implement actual snapping
  -- TODO: Coalesce duplicate snaps

  forM_ (Logic.uniqueAlignments 0.0 6.0 bboxes mouse) (uncurry referenceline)

  -- TODO: Save and push state to prevent interference, rather than resetting manually
  Cairo.newPath        --
  Cairo.setDash [] 0.0 -- Disable dashes
  where
    crosshairs (fr:+om) = Cairo.setLineWidth 1.0 >> Cairo.setDash [] 0.0 >> choose Palette.black >> Render.crosshairs (fr:+om) (22:+22) >> Cairo.stroke
    cairosetup d        = choose (markTheSpot d) >> Cairo.setLineWidth 1 >> Cairo.setDash [8, 8] 0
    markTheSpot d       = let i = if d /= 0.0 then 0.4 else 0.0 in (i, i, i, 1.0) --

    referenceline (Vertical d)   (fr:+om) = cairosetup d >> Render.line (fr:+om) (fr:+my) >> Cairo.stroke >> crosshairs (fr:+om)
    referenceline (Horizontal d) (fr:+om) = cairosetup d >> Render.line (fr:+om) (mx:+om) >> Cairo.stroke >> crosshairs (fr:+om)


-- | Renders the X and Y axes meeting at the given origin point.
-- TODO: Options
-- TODO: Hide mouse, use axes instead
-- TODO: Use crosshairs with 'hollow middle' at the origin point (?)
axes :: Complex Double -> Complex Double -> Cairo.Render ()
axes (ox:+oy) (width:+height) = do
  Cairo.setLineWidth 1
  Cairo.setDash [5, 5] 14

  choose Palette.green
  Cairo.moveTo     0 oy
  Cairo.lineTo width oy
  Cairo.stroke

  choose Palette.red
  Cairo.moveTo ox      0
  Cairo.lineTo ox height
  Cairo.stroke


-- |
debugHUD :: AppState -> Cairo.Render ()
debugHUD appstate = do
  -- Debug info (HUD)
  Cairo.moveTo 20 20
  choose Palette.darkblue
  Cairo.setFontSize 16
  -- Cairo.fontOptionsSetAntilias Cairo.AntialiasDefault
  Cairo.selectFontFace ("Helvetica" :: String) Cairo.FontSlantNormal Cairo.FontWeightNormal
  Cairo.showText $ (printf "Mouse=(%.02f, %.02f)" mx my :: String)
  maybe pass (\(px:+py) -> Cairo.showText (printf " | Size=(%.02f, %.02f)" (abs $ px-mx) (abs $ py-my) :: String)) (appstate ^. pin)
  where
    (mx:+my) = appstate ^. mouse
    msize    = liftM (dotmap abs . subtract (mx:+my)) (appstate ^. pin)
    pass     = return ()
