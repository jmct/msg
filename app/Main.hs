{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-| Compute factorials in Haskell -}
import HsLua

import LuaConfig.Dict

-- testInit :: LuaE Exception Status
testInit = openbase >> HsLua.dofile "test.lua" >> getLuaTable "test_table"

testGettingTable name = testInit >> getLuaTable name

-- loadFile, getglobal, gettop, gettable

main :: IO ()
main =
 do
  run @HsLua.Exception (openlibs >> dostring  "print('Hello, from Lua')\n")
  tab <- run @HsLua.Exception testInit
  print tab
  return ()
