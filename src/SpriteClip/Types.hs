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
-- TODO: Bad idea to export all of these modules?
module SpriteClip.Types (module SpriteClip.Types,
                         module Data.Complex,
                         module T, module S, module M,
                         module Cairo,
                         module Gtk,
                         module Southpaw.Cartesian.Plane.BoundingBox) where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Data.Complex
import qualified Data.Set  as S
import qualified Data.Text as T
import qualified Data.Map  as M

import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.UI.Gtk          as Gtk

import Southpaw.Cartesian.Plane.BoundingBox



--------------------------------------------------------------------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------------------------------------------------------------------
-- Sprite sheets ---------------------------------------------------------------------------------------------------------------------------
-- |
-- type CutoutState = ([BoundingBox Double], Complex Double, Maybe (Complex Double))


-- |
-- TODO: Split up into logical, UI, graphics, etc. (or whatever grouping makes the most sense)
-- TODO: Rename '_cutouts' (?)
-- TODO: Coordinate transformations (scaling, panning)
data AppState = AppState { _cutouts :: [BoundingBox Double],                     --
                           _active  :: Maybe (BoundingBox Double, Complex Int),  -- The active cutout and marker (or Nothing)
                           _sheet   :: Cairo.Surface,                            -- The sprite sheet image
                           _mouse   :: Complex Double,                           -- The current position of the mouse
                           _keys    :: S.Set T.Text,                             -- The set of currently pressed keys
                           _pin     :: Maybe (Complex Double)                    --
                          --  _actionQueue :: []                                 --
                         }


-- |
-- data InputState = InputState { _mouse :: Complex Double,
--                                _keys  :: S.Set Key }


-- Input -----------------------------------------------------------------------------------------------------------------------------------
-- |
-- TODO: Rename (eg. 'Command') (?)
type Command = AppState -> IO AppState
-- data Action  = Action String (AppState -> IO AppState)


-- | Maps names to commands
-- type CommandPalette = M.Map String Command


-- |
-- TODO: Rename (?)
-- TODO: Figure out a better representation (?)
-- TODO: Composite types (?)
-- TODO: Mouse move/click/relative/scroll, key up/down/hold, drag'n'drop begin/end/motion/dropped,
-- type Key = String
type Event = S.Set String


-- |
data KeyBindings = KeyBindings (M.Map Event Command)


-- |
data Settings = Settings {}


-- | UI state machine
-- TODO: Rename
-- data InteractionState = Resizing Corner BoundingBox | Moving BoundingBox | Selected BoundingBox


-- Logical ---------------------------------------------------------------------------------------------------------------------------------
-- |
data Alignment f = Vertical f | Horizontal f deriving (Show, Eq)
