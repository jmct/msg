{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
import HsLua

main :: IO ()
main =
 do
  run @HsLua.Exception (openlibs >> dostring "print('Hello, from Lua\n')\n")
  return ()
