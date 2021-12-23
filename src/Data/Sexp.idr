module Data.Sexp

import Data.List
import Data.String

import Data.Utils

%default total

public export
data Sexp : Type where
  SAtom : String -> Sexp
  SList : List Sexp -> Sexp

public export
interface ToSexp a where
  toSexp : a -> Sexp

public export
interface FromSexp a where
  fromSexp : Sexp -> Either String a

safeChar : Char -> Bool
safeChar c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_' || c == '-'

isSafe : String -> Bool
isSafe "" = False
isSafe s = let l = prim__strLength s in
               if l > 20 then False else
               go s l 0 where
                 go : String -> Int -> Int -> Bool
                 go s l i = if i >= l then True
                                      else safeChar (assert_total $ strIndex s i) && (assert_total $ go s l (i+1))

escapeChar : Char -> List Char
escapeChar '\n' = ['\\', 'n']
escapeChar '\t' = ['\\', 't']
escapeChar c = ('\\'::'{'::(unpack $ cast $ cast {to=Int} c)) ++ ['}']

safeCharInQuotes : Char -> Bool
safeCharInQuotes c = (c >= ' ' && c <= '~') && (c /= '\\') && (c /= '"')

safeShow : String -> String
safeShow s = fastPack ('"' :: reverse ('"' :: safeShow' (unpack s) []))
  where
    safeShow' : List Char -> List Char -> List Char
    safeShow' [] acc = acc
    safeShow' (c::rest) acc =
      if safeCharInQuotes c
         then safeShow' rest (c::acc)
         else safeShow' rest (reverseOnto acc (escapeChar c))

export
Show Sexp where
  show (SAtom s) = if isSafe s then s else (safeShow s)
  show (SList xs) = "(" ++ (showSep " " (assert_total $ map show xs)) ++ ")"

export
unAtom : (errmsg : String) -> Sexp -> Either String String
unAtom _ (SAtom s) = Right s
unAtom m v = Left $ "expected an atom as " ++ m ++ ", got: " ++ show v
