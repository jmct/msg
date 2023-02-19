{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module LuaConfig.Dict where

import HsLua

import Data.ByteString
import Data.Int (Int64)

import qualified Data.Map as M
import           Data.Map (Map)

import Debug.Trace (trace)

type LuaTable k v = Map k v

data LuaVal = LuaS ByteString
            | LuaB Bool
            | LuaI Int64
            | LuaN Number
            | LuaT (LuaTable LuaVal LuaVal)
  deriving (Show, Eq, Ord)

-- | Get a `LuaTable` representing the value of `name` in the Lua State.
-- if `name` is not a table, calling this function will result in a LuaError
getLuaTableName :: LuaError e => Name -> LuaE e (LuaTable LuaVal LuaVal)
getLuaTableName name =
 do typ <- getglobal name
    idx <- gettop
    -- trace ("idx: " ++ show idx) (pure ())
    if typ /= TypeTable
    then throwTypeMismatchError "Table" idx
    else getLuaTable' idx

-- | Get the `LuaTable` at the given `StackIndex`.
-- I'm not sure what this function will do if it's not actually a table.
-- I also don't intend on exposing this function as part of the public API
-- Note to future me: if you make this function public, figure out what it does
-- if the value at `idx` is not a table.
getLuaTable' :: LuaError e => StackIndex -> LuaE e (LuaTable LuaVal LuaVal)
getLuaTable' idx = pushnil >> populateMap M.empty (next idx) process
 where
    process = do
      tyk <- ltype (nth 2)
      tyv <- ltype (nth 1)
      -- trace ("tyk: " ++ show tyk) (pure ())
      -- trace ("tyv: " ++ show tyv) (pure ())
      mk <- getVal tyk (nth 2)
      -- trace ("k: " ++ show k) (pure ())
      mv <- getVal tyv (nth 1)
      elem <- pure ((\k v -> (k,v)) <$> mk <*> mv)
      -- trace ("v: " ++ show v) (pure ())
      pop 1
      pure elem

getVal :: LuaError e => Type -> StackIndex -> LuaE e (Maybe LuaVal)
getVal ty idx
 | ty == TypeBoolean = (Just . LuaB) <$> toboolean idx
 | ty == TypeString  = (fmap LuaS) <$> tostring idx
 | ty == TypeNumber  = (fmap LuaN) <$> tonumber idx
 | ty == TypeTable   =
  do
    t <- gettop >>= getLuaTable'
    pure (Just (LuaT t))
 | otherwise         = pure Nothing
      
populateMap :: LuaTable LuaVal v -> LuaE e Bool -> LuaE e (Maybe (LuaVal,v)) -> LuaE e (LuaTable LuaVal v)
populateMap tab p action = do
  b <- p
  case b of
    False -> pure tab
    True  -> do
      elem <- action
      case elem of
        Nothing     -> populateMap tab p action -- do not add this elem
        Just (k,v)  -> populateMap (M.insert k v tab) p action
