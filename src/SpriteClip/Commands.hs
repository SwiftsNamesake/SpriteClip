-- |
-- Module      : SpriteClip.Commands
-- Description :
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
--

-- Created September 11 2015

-- TODO | - Create dispatch module, move to Southpaw
--        -

-- SPEC | -
--        -



--------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Pragmas
--------------------------------------------------------------------------------------------------------------------------------------------
{-# LANGUAGE NoOverloadedLists   #-}
{-# LANGUAGE NoOverloadedStrings #-}



--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module SpriteClip.Commands where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import qualified Data.Map   as M
import qualified Data.Set   as S
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as BS

import SpriteClip.Types
import SpriteClip.Lenses
import SpriteClip.Persistence



--------------------------------------------------------------------------------------------------------------------------------------------
-- Data
--------------------------------------------------------------------------------------------------------------------------------------------
-- |
-- TODO: Use set of keys instead (?)
commands :: M.Map Event Command
commands = M.fromList [ (S.fromList["k"], \appstate -> putStrLn "You pressed 'k'" >> return appstate),
                        (S.fromList["e"], \appstate -> putStrLn "You pressed 'e'" >> return appstate),
                        (S.fromList["control_l", "s"], \appstate -> BS.writeFile path (JSON.encode $ appstate ^. cutouts) >> return appstate) ]
  where
    path = "C:/Users/Jonatan/Desktop/Haskell/projects/SpriteClip/assets/saves/spritesections.json"



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Allow events with different behaviours (eg. continous, onrelease, onpress, etc.)
-- TODO: Less confusing names
dispatch :: String -> Event -> M.Map Event Command -> [Command]
dispatch new events keymap = map snd . M.toList $ M.filterWithKey (\k v -> S.member new k && matchEvent k events) keymap


-- |
-- TODO: Rename (?)
matchEvent :: Event -> Event -> Bool
matchEvent event eventset = event `S.isSubsetOf` eventset
