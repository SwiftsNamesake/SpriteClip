-- |
-- Module      : SpriteClip.Types
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



--------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Pragmas
--------------------------------------------------------------------------------------------------------------------------------------------



--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module SpriteClip.Types (module SpriteClip.Types,
                         module Data.Complex,
                         module Cairo,
                         module Gtk,
                         module Southpaw.Cartesian.Plane.BoundingBox) where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Data.Complex

import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.UI.Gtk          as Gtk

import Southpaw.Cartesian.Plane.BoundingBox



--------------------------------------------------------------------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------------------------------------------------------------------
-- Sprite sheets ---------------------------------------------------------------------------------------------------------------------------
-- |
type CutoutState = ([BoundingBox Double], Complex Double, Maybe (Complex Double))


-- |
-- TODO: Split up into logical, UI, graphics, etc. (or whatever grouping makes the most sense)
-- TODO: Rename '_cutouts' (?)
data AppState = AppState { _cutouts :: [BoundingBox Double],                     --
                           _active  :: Maybe (BoundingBox Double, Complex Int),  -- The active cutout and marker (or Nothing)
                           _sheet   :: Cairo.Surface,                            -- The sprite sheet image
                           _mouse   :: Complex Double,                           -- The current position of the mouse
                           _pin     :: Maybe (Complex Double)                    --
                         }
