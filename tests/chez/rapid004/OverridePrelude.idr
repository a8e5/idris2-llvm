module Main

import Data.String

-- Test, that Scheme-dependent builtins are correctly blacklisted/overridden

main : IO ()
main = do
  putStrLn $ fastPack ['a', 'b', 'c']
  putStrLn $ fastConcat ["first part", "|", "", "|", "END of the string"]
  putStrLn $ fastConcat ["last", "part", "empty", ""]
