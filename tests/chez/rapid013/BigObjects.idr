module Main

import Data.IOArray
import Data.String

big : Int
big = 100000000

medium : Int
medium = 100000

makeBigString : String -> IO ()
makeBigString s = do
  putStrLn ("string length is now " ++ (show $ strLength s))
  if strLength s > big
     then putStrLn "max string length reached"
     else makeBigString (s ++ s)


main : IO ()
main = do
  med <- newArray medium
  ignore $ writeArray med 0 "first medium array element"
  ignore $ writeArray med (medium - 1) "last medium array element"
  ignore $ writeArray med (medium `div` 2) "somewhere in the middle (medium)"
  printLn =<< readArray med 0
  printLn =<< readArray med (medium - 1)
  printLn =<< readArray med (medium `div` 2)

  arr <- newArray big
  ignore $ writeArray arr 0 "first array element"
  ignore $ writeArray arr (big - 1) "last array element"
  ignore $ writeArray arr (big `div` 2) "somewhere in the middle"
  printLn =<< readArray arr 0
  printLn =<< readArray arr (big - 1)
  printLn =<< readArray arr (big `div` 2)
  makeBigString "Pi3ce"
  makeBigString "Pi3c"
  pure ()
