module Data.Utils

import Data.Bits
import Data.Buffer
import Data.Vect

export
enumerateVect : {n : Nat} -> Vect n a -> Vect n (Int, a)
enumerateVect l = enumerate' 0 l where
  enumerate' : {n : Nat} -> Int -> Vect n a -> Vect n (Int, a)
  enumerate' _ [] = []
  enumerate' i (x::xs) = (i, x)::(enumerate' (i+1) xs)

export
enumerate : List a -> List (Int, a)
enumerate l = enumerate' 0 l where
  enumerate' : Int -> List a -> List (Int, a)
  enumerate' _ [] = []
  enumerate' i (x::xs) = (i, x)::(enumerate' (i+1) xs)

hexDigit : Bits64 -> Char
hexDigit 0 = '0'
hexDigit 1 = '1'
hexDigit 2 = '2'
hexDigit 3 = '3'
hexDigit 4 = '4'
hexDigit 5 = '5'
hexDigit 6 = '6'
hexDigit 7 = '7'
hexDigit 8 = '8'
hexDigit 9 = '9'
hexDigit 10 = 'a'
hexDigit 11 = 'b'
hexDigit 12 = 'c'
hexDigit 13 = 'd'
hexDigit 14 = 'e'
hexDigit 15 = 'f'
hexDigit _ = 'X' -- TMP HACK: Ideally we'd have a bounds proof, generated below

export
asHex : Bits64 -> String
asHex 0 = "0"
asHex n = pack $ asHex' n []
  where
    asHex' : Bits64 -> List Char -> List Char
    asHex' 0 hex = hex
    asHex' n hex = asHex' (assert_smaller n (n `shiftR` 4)) (hexDigit (n .&. 0xf) :: hex)

export
asHex2 : Int -> String
asHex2 0 = "00"
asHex2 c = let s = asHex (cast {to=Bits64} c) in
               if length s == 1 then "0" ++ s else s

export
doubleToHex : Double -> String
doubleToHex d = let bytes = unsafePerformIO (do
                                buf <- (assert_total $ fromMaybe $ idris_crash "no buf") <$> newBuffer 8
                                setDouble buf 0 d
                                bufferData buf
                                ) in
                                concatMap asHex2 $ reverse bytes

export
repeatStr : String -> Nat -> String
repeatStr s 0 = ""
repeatStr s (S x) = s ++ repeatStr s x

export
showSep : String -> List String -> String
showSep sep xs = showSepGo True xs "" where
  showSepGo : Bool -> List String -> String -> String
  showSepGo first [] acc = acc
  showSepGo first (x::xs) acc = if first then showSepGo False xs (acc ++ x)
                                         else showSepGo False xs (acc ++ sep ++ x)
