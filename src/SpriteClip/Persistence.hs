-- |
-- Module      : SpriteClip.Persistence
-- Description :
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
--

-- Created September 11 2015

-- TODO | - Refactor, utilities
--        - Figure out applicative, functor
--        - Derive instances automatically (?)
--        - Move to library


-- SPEC | -
--        -



--------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Pragmas
--------------------------------------------------------------------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}


--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module SpriteClip.Persistence where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import           Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.Map    as M
import qualified Data.Vector as V
import qualified Data.Text   as T
import qualified Data.Set    as S

import Control.Applicative ((<$>), (<*>))
import Control.Monad       (mzero)

import SpriteClip.Types
import SpriteClip.Lenses hiding ((.=))



--------------------------------------------------------------------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------------------------------------------------------------------
-- Deserialise -----------------------------------------------------------------------------------------------------------------------------
-- | Dummy version of the Command type
-- TODO: Arguments to command
instance FromJSON (S.Set T.Text, T.Text) where
  parseJSON (Object obj) = (,) <$> (obj .: "keys") <*> (obj .: "action")
  parseJSON  _           = mzero


-- |
-- instance FromJSON KeyBindings where
instance FromJSON (M.Map (S.Set T.Text) T.Text) where
  parseJSON (Array a) = M.fromList <$> V.foldr append empty a -- OMFG it actually worked!
    where
      empty               = parseJSON $ Array V.empty
      append value parser = (:) <$> parseJSON value <*> parser
  parseJSON _         = mzero


instance FromJSON r => FromJSON (Complex r) where
  parseJSON (Object obj) = (:+) <$> (obj .: "x") <*> (obj .: "y")


instance FromJSON r => FromJSON (BoundingBox r) where
  parseJSON (Object obj) = BoundingBox <$> (obj .: "centre") <*> (obj .: "size")

-- Serialise -------------------------------------------------------------------------------------------------------------------------------
-- |
instance ToJSON r => ToJSON (Complex r) where
  toJSON (x:+y) = object ["x" .= toJSON x, "y" .= toJSON y]


-- |
instance ToJSON r => ToJSON (BoundingBox r) where
  toJSON bbox = object ["centre" .= toJSON (_centre bbox), "size" .= toJSON (_size bbox)]
