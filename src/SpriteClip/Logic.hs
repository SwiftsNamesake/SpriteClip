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



--------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Pragmas
--------------------------------------------------------------------------------------------------------------------------------------------



--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module SpriteClip.Logic where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Data.Maybe    (catMaybes)
import Data.List     (groupBy, nub, minimumBy, maximumBy)
import Data.Ord      (comparing)
import Data.Function (on)
import Data.Fixed

import Southpaw.Cartesian.Plane.Utilities

import SpriteClip.Types hiding (centre)
import SpriteClip.Lenses



--------------------------------------------------------------------------------------------------------------------------------------------
-- Data
--------------------------------------------------------------------------------------------------------------------------------------------
-- |
markerPositions :: RealFloat f => [Complex f]
markerPositions = concat [corners, midline, centre]


-- |
corners :: RealFloat f => [Complex f]
corners = [(-1):+1, 1:+1, 1:+(-1), (-1):+(-1)]


-- |
midline :: RealFloat f => [Complex f]
midline = [(-1):+0, 1:+0, 0:+(-1), 0:+1]


-- |
centre :: RealFloat f => [Complex f]
centre = [0:+0]



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------

-- UI logic --------------------------------------------------------------------------------------------------------------------------------
-- | Are the points closer than 'radius' to one another?
within :: RealFloat f => f -> Complex f -> Complex f -> Bool
within radius a = (< radius) . realPart . abs . subtract a


-- | Finds all points which are closer than 'radius' to the point 'p'.
neighbours :: RealFloat f => f -> [Complex f] -> Complex f -> [Complex f]
neighbours radius points p = filter (within radius p) points


-- |
-- snapToGrid :: RealFloat f => Complex f -> Complex f -> Complex f -> Complex f
-- snapToGrid origin leap pt = 0.0:+0.0


-- |
-- TODO: This might just be the most useless function in the entire codebase
snapPointToAxis :: RealFloat f => Complex f -> Complex f -> Complex f
snapPointToAxis pin point = pin - point


-- |
snapPointsToAxis :: RealFloat f => [Complex f] -> Complex f -> [Complex f]
snapPointsToAxis points p = map (snapPointToAxis p) points


-- | Produces a list of distances to a given point, filtering out those that are too far away.
-- TODO: Include the original points with the output (those that weren't culled) (?)
-- TODO: Create 'Snap' or 'Alignment' type (?)
snapNearbyPointsToAxis :: RealFloat f => f -> [Complex f] -> Complex f -> [(Alignment f, Complex f)]
snapNearbyPointsToAxis tolerance points p = concatMap makesnap . filter (closeEnough . snd) . zip points $ snapPointsToAxis points p
  where
    closeEnough (dx:+dy)  = abs dx < tolerance || abs dy < tolerance                     -- Is the point closer than 'epsilon' to any axis?
    makesnap (pt, dx:+dy) = catMaybes [trysnap Vertical pt dx, trysnap Horizontal pt dy] -- Try to snap to both axes
    trysnap axis pt x
      | abs x < tolerance = Just (axis x, pt) -- Keep the snap delta if the point is close enough
      | otherwise         = Nothing           -- Don't snap to far away axes


-- |
alignments :: RealFloat f => f -> [Complex f] -> Complex f -> [Complex f]
alignments tolerance points point = filter (\other -> let (dx:+dy) = dotmap abs (point - other) in dx <= tolerance || dy <= tolerance) points


-- | Remvoes overlapping alignments
-- TODO: Don't use epsilon (?)
coalesceAlignments :: RealFloat f => f -> [(Alignment f, Complex f)] -> Complex f -> [(Alignment f, Complex f)]
coalesceAlignments epsilon alignments pt = concatMap prune . groupBy ((==) `on` fst) $ alignments
  where
    prune als =  nub [minimumBy (comparing axisDistance) als, maximumBy (comparing axisDistance) als] -- TODO: Fix this (only allow one negative and one positive value)
    axisDistance (Horizontal _, alpt) = realPart $ pt - alpt                                          --
    axisDistance (Vertical   _, alpt) = imagPart $ pt - alpt                                          --
    -- farthesAbove, farthestBelow


-- |
-- TODO: Sort out the treatment of epsilon and tolerance
uniqueAlignments :: RealFloat f => f -> f -> [BoundingBox f] -> Complex f -> [(Alignment f, Complex f)]
uniqueAlignments epsilon tolerance bboxes pt = coalesceAlignments epsilon (snapNearbyPointsToAxis tolerance [centre bbox pt | pt <- markerPositions, bbox <- bboxes] pt) pt
  where
    centre bbox origin = let (rectx:+recty) = _size bbox in _centre bbox + dotwise (*) origin (_size bbox * 0.5) -- ((dx*rectx/2):+(dy*recty/2))


-- Utilities -------------------------------------------------------------------------------------------------------------------------------
-- |
-- TODO: Check behaviour for negative numbers
-- TODO: Better algorithm, use library function (fewer constraints) (without the if-clause)
nearestMultiple :: (Real r, Fractional r) => r -> r -> r
nearestMultiple multiple n = let distance = mod' n multiple in n - distance + if distance >= multiple*0.5 then multiple else 0


-- | Yields the minimum and maximum value in a list
extrema :: Ord n => [n] -> (n, n)
extrema xs = (minimum xs, maximum xs)


-- | Yields the minimum and maximum value in a list, as determined by the comparison function
extremaBy :: (n -> n -> Ordering) -> [n] -> (n, n)
extremaBy f xs = (minimumBy f xs, maximumBy f xs)


-- Control ---------------------------------------------------------------------------------------------------------------------------------
-- |
-- TODO: Rename (?)
applyIf :: Bool -> (a -> a) -> a -> a
applyIf True  f x = f x
applyIf False _ x = x


-- |
-- TODO: Rename (?)
applyOptionals :: [(Bool, a -> a)] -> a -> a
applyOptionals [] a = a
applyOptionals ((enabled, f):rest) a = applyOptionals rest (applyIf enabled f a)
