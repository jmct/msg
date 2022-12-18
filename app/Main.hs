{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-| Compute factorials in Haskell -}
import HsLua

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


main :: IO ()
main =
 do
  run @HsLua.Exception (openlibs >> dostring  "print('Hello, from Lua')\n")
  return ()
