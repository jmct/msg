{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module LuaConfig.Dict where

import HsLua

import Data.ByteString
import Data.Int (Int64)

import qualified Data.Map as M
import           Data.Map (Map)

type LuaTable k v = Map k v

data LuaVal = LuaS ByteString
            | LuaB Bool
            | LuaI Int64
            | LuaN Number
            | LuaT (LuaTable LuaVal (Maybe LuaVal))
  deriving (Show, Eq, Ord)

-- getLuaVal :: Name -> LuaE Exception LuaVal
-- getLuaVal name =
--  do typ <- getglobal name

getLuaTable :: Name -> LuaE Exception (LuaTable LuaVal (Maybe LuaVal))
getLuaTable name =
 do typ <- getglobal name
    if typ /= TypeTable
    then gettop >>= throwTypeMismatchError "Table"
    else do
      idx <- gettop
      pushnil
      populateHash M.empty (next idx) process
 where
    process = do
      tyk <- ltype (nth 2)
      tyv <- ltype (nth 1)
      k <- getVal tyk (nth 2)
      v <- getVal tyv (nth 1)
      pop 1
      pure (k,v)

getVal :: LuaError e => Type -> StackIndex -> LuaE e (Maybe LuaVal)
getVal ty idx
 | ty == TypeBoolean = (Just . LuaB) <$> toboolean idx
 | ty == TypeString  = (fmap LuaS) <$> tostring idx
 | ty == TypeNumber  = (fmap LuaN) <$> tonumber idx
-- | ty == TypeTable   = (fmap LuaT) <$> getLuaTable idx
 | otherwise         = pure Nothing
      
populateHash :: LuaTable LuaVal v -> LuaE e Bool -> LuaE e (Maybe LuaVal,v) -> LuaE e (LuaTable LuaVal v)
populateHash tab p action = do
  b <- p
  case b of
    False -> pure tab
    True  -> do
      (k,v) <- action
      case k of
        Nothing -> undefined -- failLua "TODO"
        Just k  -> populateHash (M.insert k v tab) p action
