-- |
-- Module      : SpriteClip.Logic
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



-------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Pragmas
-------------------------------------------------------------------------------------------------------------------------------------------



-------------------------------------------------------------------------------------------------------------------------------------------
-- API
-------------------------------------------------------------------------------------------------------------------------------------------
module SpriteClip.Logic where



-------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
-------------------------------------------------------------------------------------------------------------------------------------------
import Southpaw.Cartesian.Plane.Utilities

import SpriteClip.Types
import SpriteClip.Lenses



-------------------------------------------------------------------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------------------------------------------------------------------
-- |
markerPositions :: RealFloat f => [(f, f)]
markerPositions = concat [corners, midline, centre]


-- |
corners :: RealFloat f => [(f, f)]
corners = [(-1, 1), (1, 1), (1, -1), (-1, -1)]


-- |
midline :: RealFloat f => [(f, f)]
midline = [(-1, 0), (1, 0), (0, -1), ( 0,  1)]


-- |
centre :: RealFloat f => [(f, f)]
centre = [( 0, 0)]



-------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------------------------------------------------------------------

-- UI logic --------------------------------------------------------------------------------------------------------------------------------
-- | Finds all points which are closer than 'radius' to the point 'p'.
neighbours :: RealFloat f => f -> [Complex f] -> Complex f -> [Complex f]
neighbours radius points p = filter ((< radius) . realPart . abs . subtract p) points


-- |
snapPointToAxis :: RealFloat f => Complex f -> Complex f -> Complex f
snapPointToAxis pin point = pin - point


-- |
snapPointsToAxis :: RealFloat f => [Complex f] -> Complex f -> [Complex f]
snapPointsToAxis points p = map (snapPointToAxis p) points


-- | Produces a list of distances to a given point, filtering out those that are too far away.
-- TODO: Include the original points with the output (those that weren't culled) (?)
snapNearbyPointsToAxis :: RealFloat f => f -> [Complex f] -> Complex f -> [(Maybe f, Maybe f, Complex f)]
snapNearbyPointsToAxis tolerance points p = map makesnap . filter (closeEnough . snd) . zip points $ snapPointsToAxis points p
  where
    closeEnough (dx:+dy)  = abs dx < tolerance || abs dy < tolerance -- Is the point closer than 'epsilon' to any axis?
    makesnap (pt, dx:+dy) = (trysnap dx, trysnap dy, pt)             -- (Maybe snap dx, Maybe snap dy, original point)
    trysnap x
      | abs x < tolerance = Just x  -- Keep the snap delta if the point is close enough
      | otherwise         = Nothing -- Don't snap to far away axes


-- |
alignments :: RealFloat f => f -> [Complex f] -> Complex f -> [Complex f]
alignments tolerance points point = filter (\other -> let (dx:+dy) = dotmap abs (point - other) in dx <= tolerance || dy <= tolerance) points
