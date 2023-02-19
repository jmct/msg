module Data.Site where

import qualified Data.Map as M
import           Data.Map (Map)

import Path

import LuaConfig.Dict

data Site = S { root :: Path Rel Dir
              , dirs :: [SubDir]
              }
 deriving Show

data SubDir = SD { source :: Path Rel Dir
                 , conversions :: Map FT FT
                 }
 deriving Show

data FT = LHS | MD | HTML
 deriving Show

mkSite :: LuaTable LuaVal LuaVal -> Either String Site
mkSite   = undefined
