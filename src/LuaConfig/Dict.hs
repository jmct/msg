{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module LuaConfig.Dict where

import HsLua
import Data.ByteString
import Data.Int (Int64)

data LuaVal = LuaS ByteString
            | LuaI Int64
            | LuaN Double
            | LuaT
  deriving Show

testInit :: LuaE Exception Status
testInit = openlibs >> HsLua.dofile "test.lua"

toAsscL :: StackIndex -> LuaE Exception [(Maybe HsLua.Integer, Maybe HsLua.Integer)]
toAsscL idx = pushnil >> f (next idx) act
  where
    act = do
      k <- tointeger (nth 2)
      v <- tointeger (nth 1)
      pop 1
      pure (k,v)

f :: LuaE e Bool -> LuaE e a -> LuaE e [a]
f p action = do
  b <- p
  case b of
    False -> pure []
    True  -> do
      x <- action
      (x:) <$> f p action
