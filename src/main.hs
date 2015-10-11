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

-- TODO | - Refactor, break up into modules
--        -

-- SPEC | -
--        -



--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module Main where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map             as M
import qualified Data.Text            as T
import qualified Data.Aeson           as JSON
import Control.Monad (liftM)

import qualified SpriteClip.Window      as Window
import qualified SpriteClip.Persistence as Persistence



--------------------------------------------------------------------------------------------------------------------------------------------
-- Entry point
--------------------------------------------------------------------------------------------------------------------------------------------
-- |
main :: IO ()
main = do
  bindings <- (liftM JSON.eitherDecode $ BS.readFile fn) -- :: IO (Eut (M.Map T.Text T.Text))
  -- bindings <- BS.readFile "assets/preferences/keybindings.json"
  print (bindings :: Either String (M.Map T.Text T.Text))
  Window.spriteClipMain
  where
    fn = "C:/Users/Jonatan/Desktop/Haskell/projects/assets/preferences/keybindings.json"
