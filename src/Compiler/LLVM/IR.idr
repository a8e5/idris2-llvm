module Compiler.LLVM.IR

import Core.Name

import Control.Codegen
import Data.Utils

isSafeChar : Char -> Bool
isSafeChar '.' = True
isSafeChar '_' = True
isSafeChar c = isAlphaNum c

fullShow : Name -> String
fullShow (NS ns n) = showNSWithSep "." ns ++ "." ++ fullShow n
fullShow (UN (Basic n)) = n
fullShow (UN (Field n)) = "_.(" ++ n ++ ")"
fullShow (UN Underscore) = "_"
fullShow (MN n i) = "_{" ++ n ++ ":" ++ show i ++ "}"
fullShow (PV n i) = "_{P:" ++ fullShow n ++ ":" ++ show i ++ "}"
fullShow (DN _ n) = fullShow n
fullShow (Nested (outer, idx) inner) = show outer ++ "/" ++ show idx ++ "/" ++ fullShow inner
fullShow (CaseBlock outer i) = "case/" ++ outer ++ "$" ++ show i
fullShow (WithBlock outer i) = "with/" ++ outer ++ "$" ++ show i
fullShow (Resolved i) = "resolved/" ++ show i

export
safeName : Name -> String
safeName s = concatMap okchar (unpack $ fullShow s)
  where
    okchar : Char -> String
    okchar c = if isSafeChar c
                  then cast c
                  else "$" ++ asHex (cast {to=Bits64} c)

public export
interface ToIR a where
  toIR : a -> String
  showWithoutType : a -> String

export
assignSSA : ToIR a => a -> Codegen String
assignSSA value = do
  i <- getUnique
  let varname = "%t" ++ show i
  appendCode ("  " ++ varname ++ " = " ++ (toIR value))
  pure varname

public export
data IRType = I1 | I8 | I16 | I32 | I64 | F64 | FuncPtr | IRObjPtr | Pointer Int IRType

export
Show IRType where
  show I1 = "i1"
  show I8 = "i8"
  show I16 = "i16"
  show I32 = "i32"
  show I64 = "i64"
  show F64 = "double"
  show FuncPtr = "%FuncPtr"
  show IRObjPtr = "%ObjPtr"
  show (Pointer i t) = (show t) ++ " addrspace(" ++ show i ++ ")*"

public export
data IRLabel = MkLabel String

public export
ToIR IRLabel where
  toIR (MkLabel l) = "label %" ++ l
  showWithoutType (MkLabel l) = "%" ++ l

export
beginLabel : IRLabel -> Codegen ()
beginLabel (MkLabel l) = appendCode (l ++ ":")

export
genLabel : String -> Codegen IRLabel
genLabel s = MkLabel <$> mkVarName ("glbl_" ++ s)

export
RuntimePtr : IRType
RuntimePtr = Pointer 0 I8

public export
data IRValue : IRType -> Type where
  Const : (t : IRType) -> Integer -> IRValue t
  ConstI64 : Integer -> IRValue I64
  ConstF64 : Double -> IRValue F64
  SSA : (t : IRType) ->  String -> IRValue t
  IRDiscard : IRValue (Pointer 0 IRObjPtr)

export
total
ToIR (IRValue t) where
  toIR {t} (SSA t s) = (show t) ++ " " ++ s
  toIR (Const t i) = (show t) ++ " " ++ (show i)
  toIR (ConstI64 i) = "i64 " ++ (show i)
  toIR (ConstF64 f) = "double 0x" ++ (assert_total $ doubleToHex f)
  toIR (IRDiscard) = "ERROR: trying to use DISCARD with type"

  showWithoutType (SSA _ n) = n
  showWithoutType (Const _ i) = show i
  showWithoutType (ConstI64 i) = show i
  showWithoutType (ConstF64 f) = "0x" ++ (assert_total $ doubleToHex f)
  showWithoutType (IRDiscard) = "ERROR: trying to use DISCARD without type"

export
nullPtr : IRValue IRObjPtr
nullPtr = SSA IRObjPtr "null"

export
pConst : Cast a Integer => {ty : IRType} -> a -> IRValue ty
pConst {ty} val = Const ty (cast {to=Integer} val)
