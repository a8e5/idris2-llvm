module Data.Utils

import Data.Bits
import Data.Vect

export
enumerateVect : {n : Nat} -> Vect n a -> Vect n (Int, a)
enumerateVect l = enumerate' 0 l where
  enumerate' : {n : Nat} -> Int -> Vect n a -> Vect n (Int, a)
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
