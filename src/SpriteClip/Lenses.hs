-- |
-- Module      : SpriteClip.Lenses
-- Description : Defines synonyms as well as new lenses for the types in this package.
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
--

-- Created September 7 2015

-- TODO | - Find a way to resolve or circumvent the Cairo DLL error caused by using the makeLenses template
--        -

-- SPEC | -
--        -



-------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Pragmas
-------------------------------------------------------------------------------------------------------------------------------------------



-------------------------------------------------------------------------------------------------------------------------------------------
-- API
-------------------------------------------------------------------------------------------------------------------------------------------
module SpriteClip.Lenses (module SpriteClip.Lenses,
                          module Control.Lens) where



-------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
-------------------------------------------------------------------------------------------------------------------------------------------
import Control.Lens
import qualified Graphics.Rendering.Cairo as Cairo

import SpriteClip.Types



-------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------
-- |
cutouts :: Lens AppState AppState [BoundingBox Double] [BoundingBox Double]
cutouts f s = (\new -> s { _cutouts=new }) `fmap` f (_cutouts s)


-- |
active :: Lens AppState AppState (Maybe (BoundingBox Double, Complex Int)) (Maybe (BoundingBox Double, Complex Int))
active f s = (\new -> s { _active=new }) `fmap` f (_active s)


-- |
sheet :: Lens AppState AppState Cairo.Surface Cairo.Surface
sheet f s = (\new -> s { _sheet=new }) `fmap` f (_sheet s)


-- |
mouse :: Lens AppState AppState (Complex Double) (Complex Double)
mouse f s = (\new -> s { _mouse=new }) `fmap` f (_mouse s)


-- |
pin :: Lens AppState AppState (Maybe (Complex Double)) (Maybe (Complex Double))
pin f s = (\new -> s { _pin=new }) `fmap` f (_pin s)


-- makeLens :: (AppState -> b) -> Lens AppState AppState b b
-- makeLens prop f s = (\new -> s { prop=new }) `fmap` f (prop s)


-- Lenses -------------------------------------------------------------------------------------------------------------
-- cutouts' :: Field1 s t a b
-- cutouts' :: (Field1 CutoutState CutoutState [BoundingBox Double] [BoundingBox Double]) a => a
-- cutouts' :: ([BoundingBox Double] -> Identity [BoundingBox Double]) -> CutoutState -> Identity CutoutState
cutouts' :: (Field1 s t a b, Functor f) => (a -> f b) -> s -> f t
cutouts' = _1


-- mouse' :: Field2 s t a b
-- mouse' :: (Complex Double -> Identity (Complex Double)) -> CutoutState -> Identity CutoutState
mouse' :: (Field2 s t a b, Functor f) => (a -> f b) -> s -> f t
mouse' = _2


-- click' :: Field3 s t a b
-- click' :: Field1 CutoutState CutoutState (Maybe (Complex Double)) (Maybe (Complex Double))
-- click' :: (Maybe (Complex Double) -> Identity (Maybe (Complex Double))) -> CutoutState -> Identity CutoutState
click' :: (Field3 s t a b, Functor f) => (a -> f b) -> s -> f t
click' = _3
