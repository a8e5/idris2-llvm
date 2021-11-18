module Main

import Data.IOArray
import Data.String

big : Int
big = 200000

huge : Int
huge = 100000000

medium : Int
medium = 100000

small : Int
small = 4000

makeBigString : String -> IO ()
makeBigString s = do
  putStrLn ("string length is now " ++ (show $ strLength s))
  if strLength s > huge
     then putStrLn "max string length reached"
     else makeBigString (s ++ s)


main : IO ()
main = do
  arrLvl1 <- newArray small
  arrLvl2 <- newArray small
  ignore $ writeArray arrLvl1 42 arrLvl2
  arrLvl3 <- newArray small
  ignore $ writeArray arrLvl2 42 arrLvl3
  ignore $ writeArray arrLvl3 42 "The MAGIC string"

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

  (Just readLvl2) <- readArray arrLvl1 42
    | Nothing => printLn "ERROR: lvl2 not found in lvl1"
  (Just readLvl3) <- readArray readLvl2 42
    | Nothing => printLn "ERROR: lvl3 not found in lvl2"
  (Just result) <- readArray readLvl3 42
    | Nothing => printLn "ERROR: result not found in lvl3"
  printLn result
