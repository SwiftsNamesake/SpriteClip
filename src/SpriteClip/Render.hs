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
--        -

-- SPEC | -
--        -



--------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Pragmas
--------------------------------------------------------------------------------------------------------------------------------------------



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

import Text.Printf
import Control.Monad (forM_, when)

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
render :: Complex Double -> Cairo.Surface -> [BoundingBox Double] -> Maybe (Complex Double) -> Complex Double -> Cairo.Render ()
render canvasSize sheet sections' mclick mouse = do
  Render.image (canvasSize*(0.5:+0.0)) sheet -- Render sprite sheet
  -- Cairo.liftIO $ print (length $ take 5 sections)
  forM_ sections (section mouse)         -- Render sprite sections with markers
  forM_ sections (flip alignments mouse) -- Render alignments
  axes mouse canvasSize                  -- Render cursor axes
  debugHUD mouse                         -- Render debug overlay
  where
    sections = sections' ++ maybe [] (\click -> [BBox.fromCorners click mouse]) mclick


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
sectionMarkers :: BoundingBox Double -> Complex Double -> Cairo.Render ()
sectionMarkers bbox mouse = do
  forM_ markerBunches $ \ (radius, colour, offsets) ->
    forM_ offsets $ \(dx, dy) -> do
      choose colour
      let scale   = max 0.3 $ min 1.6 (let szx:+szy = _size bbox in min (abs szx) (abs szy)/120.0) -- Scale depending on section size
          focused = (< scale*radius) . realPart $ abs (mouse - centre (dx:+dy))                    -- Does the cursor lie on the marker?
          r       = scale*radius*(if focused then 1.4 else 1.0)                                    --
      Render.circle (centre $ dx:+dy) r
      Cairo.fill
  where
    (rectx:+recty)  = _size bbox
    centre (dx:+dy) = _centre bbox + ((dx*rectx/2):+(dy*recty/2))
    markerBunches   = [(12, Palette.orange & alphapart .~ 0.7, Logic.corners),
                       (10, Palette.peru   & alphapart .~ 0.7, Logic.midline),
                       ( 7, Palette.plum   & alphapart .~ 0.7, Logic.centre)]


-- | Renders the alignments for a sprite section (respresented as a 'BoundingBox')
-- TODO: Don't assume the point represents the mouse
alignments :: BoundingBox Double -> Complex Double -> Cairo.Render ()
alignments bbox mouse@(mx:+my) = do

  -- TODO: Implement actual snapping
  -- TODO: Coalesce duplicate snaps
  choose (0.4, 0.4, 0.4, 1.0)
  Cairo.setLineWidth 1
  Cairo.setDash [8, 8] 0

  forM_ (Logic.snapNearbyPointsToAxis 6.0 [centre $ dx:+dy | (dx, dy) <- Logic.markerPositions] mouse) $ \(mdx, mdy, p@(x:+y)) -> do
    maybe pass (const $ referenceline (x:+my) p) mdx
    maybe pass (const $ referenceline (mx:+y) p) mdy

  -- TODO: Save and push state to prevent interference, rather than resetting manually
  Cairo.newPath        --
  Cairo.setDash [] 0.0 -- Disable dashes
  where
    (rectx:+recty)  = _size bbox
    centre (dx:+dy) = _centre bbox + ((dx*rectx/2):+(dy*recty/2))
    pass            = return ()
    referenceline (t:+o) (fr:+om) = Cairo.moveTo fr om >> Cairo.lineTo t o >> Cairo.stroke


-- | Renders the X and Y axes meeting at the given origin point.
-- TODO: Options
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
debugHUD :: Complex Double -> Cairo.Render ()
debugHUD (mx:+my) = do
  -- Debug info (HUD)
  Cairo.moveTo 20 20
  choose Palette.darkblue
  Cairo.setFontSize 16
  -- Cairo.fontOptionsSetAntilias Cairo.AntialiasDefault
  Cairo.selectFontFace "Helvetica" Cairo.FontSlantNormal Cairo.FontWeightNormal
  Cairo.showText $ (printf "X=%.02f | Y=%.02f" mx my :: String)
