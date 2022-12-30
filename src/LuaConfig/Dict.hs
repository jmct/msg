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
            | LuaT (LuaTable LuaVal (Maybe LuaVal))
  deriving (Show, Eq, Ord)

-- getLuaVal :: Name -> LuaE Exception LuaVal
-- getLuaVal name =
--  do typ <- getglobal name

getLuaTable' :: LuaError e => StackIndex -> LuaE e (LuaTable LuaVal (Maybe LuaVal))
getLuaTable' idx = pushnil >> populateHash M.empty (next idx) process
 where
    process = do
      tyk <- ltype (nth 2)
      tyv <- ltype (nth 1)
      trace ("tyk: " ++ show tyk) (pure ())
      trace ("tyb: " ++ show tyv) (pure ())
      k <- getVal tyk (nth 2)
      v <- getVal tyv (nth 1)
      pop 1
      pure (k,v)

getLuaTable :: LuaError e => Name -> LuaE e (LuaTable LuaVal (Maybe LuaVal))
getLuaTable name =
 do typ <- getglobal name
    idx <- gettop
    trace ("idx: " ++ show idx) (pure ())
    if typ /= TypeTable
    then throwTypeMismatchError "Table" idx
    else getLuaTable' idx

getVal :: LuaError e => Type -> StackIndex -> LuaE e (Maybe LuaVal)
getVal ty idx
 | ty == TypeBoolean = (Just . LuaB) <$> toboolean idx
 | ty == TypeString  = (fmap LuaS) <$> tostring idx
 | ty == TypeNumber  = (fmap LuaN) <$> tonumber idx
 | ty == TypeTable   = (Just . LuaT) <$> getLuaTable' idx
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
