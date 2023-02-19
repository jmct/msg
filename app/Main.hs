{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-| Compute factorials in Haskell -}
import HsLua

import LuaConfig.Dict

import qualified Data.Text    as T
import qualified Data.Text.IO as T

import Text.Pandoc

lhsOptions = def { readerExtensions = extensionsFromList [Ext_literate_haskell] }

-- testInit :: LuaE Exception Status
testInit = openbase >> HsLua.dofile "test.lua" >> getLuaTableName "site"

testGettingTable name = testInit >> getLuaTableName name

-- loadFile, getglobal, gettop, gettable

main :: IO ()
main =
 do
  conts <- T.readFile "Test.lhs"
  out   <- runIO $ readMarkdown lhsOptions conts >>= writeHtml5String def
  case out of
    Left e  -> print e
    Right c -> T.writeFile "test.html" c
  run @HsLua.Exception (openlibs >> dostring  "print('Hello, from Lua')\n")
  tab <- run @HsLua.Exception testInit
  print tab
  return ()
