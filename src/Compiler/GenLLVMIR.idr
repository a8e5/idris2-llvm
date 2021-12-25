module Compiler.GenLLVMIR

import Data.Either
import Data.List
import Data.List1
import Data.Maybe
import Data.String
import Data.Vect

import Compiler.CompileExpr
import Compiler.VMCode
import Compiler.LLVM.IR
import Compiler.LLVM.Instruction
import Compiler.LLVM.Rapid.Closure
import Compiler.LLVM.Rapid.Integer
import Compiler.LLVM.Rapid.Builtin
import Compiler.LLVM.Rapid.Foreign
import Compiler.LLVM.Rapid.Object
import Compiler.LLVM.Rapid.String
import Control.Codegen
import Core.TT
import Data.Utils
import Libraries.Data.SortedMap
import Rapid.Common

-- work around Idris issue #2032: Slow typechecking on Int operation when Data.Fin.fromInteger is in scope
%hide Data.Fin.fromInteger

-- we provide our own in Data.Utils
%hide Core.Name.Namespace.showSep

ToIR Reg where
  toIR (Loc i) = "%v" ++ show i
  toIR RVal = "%rval"
  toIR Discard = "undef"

  showWithoutType (Loc i) = "%v" ++ show i
  showWithoutType RVal = "%rval"
  showWithoutType Discard = "undef"

argIR : Reg -> Codegen String
argIR (Loc i) = pure $ "%ObjPtr %v" ++ show i
argIR _ = pure $ "undef"

isReturn : Reg -> Bool
isReturn RVal = True
isReturn _ = False

reg2val : Reg -> IRValue (Pointer 0 IRObjPtr)
reg2val (Loc i) = SSA (Pointer 0 IRObjPtr) ("%v" ++ show i ++ "Var")
reg2val RVal = SSA (Pointer 0 IRObjPtr) ("%rvalVar")
reg2val Discard = IRDiscard

unboxBits64 : IRValue IRObjPtr -> Codegen (IRValue I64)
unboxBits64 bits64Obj = getObjectSlot bits64Obj 0

unboxDouble : IRValue IRObjPtr -> Codegen (IRValue F64)
unboxDouble doubleObj = getObjectSlot doubleObj 0

unboxInt : IRValue (Pointer 0 IRObjPtr) -> Codegen (IRValue I64)
unboxInt src = unboxInt' !(load src)

unboxIntSigned' : Int -> IRValue IRObjPtr -> Codegen (IRValue I64)
unboxIntSigned' 8 src = mkSext =<< (mkTrunc {to=I8} =<< unboxInt' src)
unboxIntSigned' 16 src = mkSext =<< (mkTrunc {to=I16} =<< unboxInt' src)
unboxIntSigned' 32 src = mkSext =<< (mkTrunc {to=I32} =<< unboxInt' src)
unboxIntSigned' bits _ = do
  addError ("not a small int kind: " ++ show bits ++ " bits")
  mkRuntimeCrash ("not a small int kind: " ++ show bits ++ " bits")
  pure (Const I64 0)

unboxIntSigned : Int -> IRValue (Pointer 0 IRObjPtr) -> Codegen (IRValue I64)
unboxIntSigned bits reg = unboxIntSigned' bits !(load reg)

intToBits64' : IRValue IRObjPtr -> Codegen (IRValue IRObjPtr)
intToBits64' val = do
  ival <- unboxInt' val
  truncatedVal <- mkAnd (Const I64 0xffffffffffffffff) ival
  cgMkBits64 truncatedVal

total
showConstant : Constant -> String
showConstant (I i) = "(I " ++ show i ++ ")"
showConstant (BI i) = "(BI " ++ show i ++ ")"
showConstant (Str i) = "(Str " ++ show i ++ ")"
showConstant (Ch i) = "(Ch " ++ show i ++ ")"
showConstant (Db i) = "(Db " ++ show i ++ ")"
showConstant (B8 i)  = "(B8 " ++ show i ++ ")"
showConstant (B16 i) = "(B16 " ++ show i ++ ")"
showConstant (B32 i) = "(B32 " ++ show i ++ ")"
showConstant (B64 i) = "(B64 " ++ show i ++ ")"
showConstant (I8 i)  = "(I8 " ++ show i ++ ")"
showConstant (I16 i) = "(I16 " ++ show i ++ ")"
showConstant (I32 i) = "(I32 " ++ show i ++ ")"
showConstant (I64 i) = "(I64 " ++ show i ++ ")"
showConstant other = "(CONST " ++ show other ++ ")"

makeConstCaseLabelName : String -> Constant -> String
makeConstCaseLabelName caseId (I i)   = caseId ++ "_is_" ++ show i
makeConstCaseLabelName caseId (I8 i)  = caseId ++ "_is_" ++ show i
makeConstCaseLabelName caseId (I16 i) = caseId ++ "_is_" ++ show i
makeConstCaseLabelName caseId (I32 i) = caseId ++ "_is_" ++ show i
makeConstCaseLabelName caseId (I64 i) = caseId ++ "_is_" ++ show i
makeConstCaseLabelName caseId (B8 i)  = caseId ++ "_is_" ++ show i
makeConstCaseLabelName caseId (B16 i) = caseId ++ "_is_" ++ show i
makeConstCaseLabelName caseId (B32 i) = caseId ++ "_is_" ++ show i
makeConstCaseLabelName caseId (B64 i) = caseId ++ "_is_" ++ show i
makeConstCaseLabelName caseId (Ch c)  = caseId ++ "_is_" ++ show i where i:Int; i = (cast {to=Int} c)
makeConstCaseLabelName caseId c       = "const case error: " ++ (showConstant c)

makeConstCaseLabel : String -> (Constant, a) -> String
makeConstCaseLabel caseId (I i,_) = "i64 " ++ show i ++ ", label %" ++ caseId ++ "_is_" ++ show i
makeConstCaseLabel caseId (I8 i,_)  = "i64 " ++ show i ++ ", label %" ++ caseId ++ "_is_" ++ show i
makeConstCaseLabel caseId (I16 i,_) = "i64 " ++ show i ++ ", label %" ++ caseId ++ "_is_" ++ show i
makeConstCaseLabel caseId (I32 i,_) = "i64 " ++ show i ++ ", label %" ++ caseId ++ "_is_" ++ show i
makeConstCaseLabel caseId (I64 i,_) = "i64 " ++ show i ++ ", label %" ++ caseId ++ "_is_" ++ show i
makeConstCaseLabel caseId (B8 i,_)  = "i64 " ++ show i ++ ", label %" ++ caseId ++ "_is_" ++ show i
makeConstCaseLabel caseId (B16 i,_) = "i64 " ++ show i ++ ", label %" ++ caseId ++ "_is_" ++ show i
makeConstCaseLabel caseId (B32 i,_) = "i64 " ++ show i ++ ", label %" ++ caseId ++ "_is_" ++ show i
makeConstCaseLabel caseId (B64 i,_) = "i64 " ++ show i ++ ", label %" ++ caseId ++ "_is_" ++ show i
makeConstCaseLabel caseId (Ch c,_) = "i32 " ++ show i ++ ", label %" ++ caseId ++ "_is_" ++ show i where i:Int; i = (cast {to=Int} c)
makeConstCaseLabel caseId (c,_) = "const case error: " ++ (showConstant c)

makeNameId : Int -> Int
makeNameId nid = 0x80000000 + nid

makeCaseLabel : {auto conNames : SortedMap Name Int} -> String -> (Either Int Name, a) -> Codegen String
makeCaseLabel caseId (Left i,_) = pure $ "i64 " ++ show i ++ ", label %" ++ caseId ++ "_tag_is_" ++ show i
makeCaseLabel {conNames} caseId (Right n,_) =
  case lookup n conNames of
       Just nameId => pure $ "i64 " ++ show (makeNameId nameId) ++ ", label %" ++ caseId ++ "_name_is_" ++ show (makeNameId nameId)
       Nothing => do addError $ "name not found: " ++ show n
                     pure "error"

instrAsComment : VMInst -> String
instrAsComment instruction = ";" ++ (unwords $ lines $ show instruction)

prepareArg : Reg -> Codegen (IRValue IRObjPtr)
prepareArg Discard = do
  pure nullPtr
prepareArg r@(Loc _) = load (reg2val r)
prepareArg RVal = do
  addError "cannot use rval as call arg"
  pure nullPtr

data ConstCaseType = IntLikeCase Constant | BigIntCase | StringCase | CharCase

total
findConstCaseType : List (Constant, List VMInst) -> Either String ConstCaseType
findConstCaseType [] = Left "empty const case"
findConstCaseType ((I _,_)::_) = pure (IntLikeCase IntType)
findConstCaseType ((I8 _,_)::_) = pure (IntLikeCase Int8Type)
findConstCaseType ((I16 _,_)::_) = pure (IntLikeCase Int16Type)
findConstCaseType ((I32 _,_)::_) = pure (IntLikeCase Int32Type)
findConstCaseType ((I64 _,_)::_) = pure (IntLikeCase Int64Type)
findConstCaseType ((B8 _,_)::_) = pure (IntLikeCase Bits8Type)
findConstCaseType ((B16 _,_)::_) = pure (IntLikeCase Bits16Type)
findConstCaseType ((B32 _,_)::_) = pure (IntLikeCase Bits32Type)
findConstCaseType ((B64 _,_)::_) = pure (IntLikeCase Bits64Type)
findConstCaseType ((BI _,_)::_) = pure BigIntCase
findConstCaseType ((Str _,_)::_) = pure StringCase
findConstCaseType ((Ch _,_)::_) = pure CharCase
findConstCaseType ((c,_)::_) = Left $ "unknown const case type: " ++ (showConstant c)

unboxChar : IRValue (Pointer 0 IRObjPtr) -> Codegen (IRValue I32)
unboxChar objPtr = do
  hdr <- getObjectHeader !(load objPtr)
  chVal64 <- mkAnd (ConstI64 0xffffffff) hdr
  chVal32 <- mkTrunc {to=I32} chVal64
  pure chVal32

assertObjectType : Reg -> Int -> Codegen ()
assertObjectType r t = assertObjectType' !(load (reg2val r)) t

mutual
getInstForConstCaseChar : {auto conNames : SortedMap Name Int} -> Reg -> List (Constant, List VMInst) -> Maybe (List VMInst) -> Codegen ()
getInstForConstCaseChar r alts def =
  do let def' = fromMaybe [(ERROR $ "no default in const case (char)")] def
     assertObjectType r OBJECT_TYPE_ID_CHAR
     caseId <- mkVarName "case_"
     let labelEnd = caseId ++ "_end"
     scrutinee <- unboxChar (reg2val r)
     appendCode $ "  switch " ++ toIR scrutinee ++ ", label %" ++ caseId ++ "_default [ " ++ (showSep "\n      " (map (makeConstCaseLabel caseId) alts)) ++ " ]"
     appendCode $ caseId ++ "_default:"
     traverse_ getInstIRWithComment def'
     appendCode $ "br label %" ++ labelEnd
     traverse_ (makeCaseAlt caseId) alts
     appendCode $ labelEnd ++ ":"
     pure ()
  where
    makeCaseAlt : String -> (Constant, List VMInst) -> Codegen ()
    makeCaseAlt caseId (Ch ch, is) = do
      let c = cast {to=Int} ch
      appendCode $ caseId ++ "_is_" ++ (show c) ++ ":"
      traverse_ getInstIRWithComment is
      appendCode $ "br label %" ++ caseId ++ "_end"
    makeCaseAlt _ (c, _) = appendCode $ "ERROR: constcase must be Char, got: " ++ show c

getInstForConstCaseString : {auto conNames : SortedMap Name Int} -> Reg -> List (Constant, List VMInst) -> Maybe (List VMInst) -> Codegen ()
getInstForConstCaseString r alts def =
  do let def' = fromMaybe [(ERROR $ "no default in const case (string)")] def
     assertObjectType r OBJECT_TYPE_ID_STR
     scrutinee <- load (reg2val r)
     let numAlts = enumerate alts
     caseId <- mkVarName "case_"
     labelEnd <- genLabel $ caseId ++ "_end"

     traverse_ (makeCaseAlt caseId labelEnd scrutinee) numAlts

     labelDefault <- genLabel $ caseId ++ "_default"
     appendCode $ "br " ++ toIR labelDefault
     beginLabel labelDefault

     traverse_ getInstIRWithComment def'
     appendCode $ "br " ++ toIR labelEnd

     beginLabel labelEnd
  where
    makeCaseAlt : String -> IRLabel -> IRValue IRObjPtr -> (Int, Constant, List VMInst) -> Codegen ()
    makeCaseAlt caseId labelEnd scrutinee (idx, Str s, is) = do
      let labelAltStart = MkLabel (caseId ++ "_alt_" ++ show idx)
      let labelAltNext = MkLabel (caseId ++ "_next" ++ show idx)
      compStr <- mkStr s
      match <- stringEqual compStr scrutinee
      appendCode $ "br " ++ toIR match ++ ", " ++ toIR labelAltStart ++ ", " ++ toIR labelAltNext
      -- compare s == scrut
      beginLabel labelAltStart
      traverse_ getInstIRWithComment is
      appendCode $ "br " ++ toIR labelEnd
      beginLabel labelAltNext
    makeCaseAlt _ _ _ (_, c, _) = appendCode $ "ERROR: constcase must be Str, got: " ++ show c

getInstForConstCaseInteger : {auto conNames : SortedMap Name Int} -> Reg -> List (Constant, List VMInst) -> Maybe (List VMInst) -> Codegen ()
getInstForConstCaseInteger r alts def =
  do let def' = fromMaybe [(ERROR $ "no default in const case (Integer)")] def
     assertObjectType r OBJECT_TYPE_ID_BIGINT
     scrutinee <- load (reg2val r)
     let numAlts = enumerate alts
     caseId <- mkVarName "case_"
     labelEnd <- genLabel $ caseId ++ "_end"

     traverse_ (makeCaseAlt caseId labelEnd scrutinee) numAlts

     labelDefault <- genLabel $ caseId ++ "_default"
     appendCode $ "br " ++ toIR labelDefault
     beginLabel labelDefault

     traverse_ getInstIRWithComment def'
     appendCode $ "br " ++ toIR labelEnd

     beginLabel labelEnd
  where
    makeCaseAlt : String -> IRLabel -> IRValue IRObjPtr -> (Int, Constant, List VMInst) -> Codegen ()
    makeCaseAlt caseId labelEnd scrutinee (idx, BI bi, is) = do
      let labelAltStart = MkLabel (caseId ++ "_alt_" ++ show idx)
      let labelAltNext = MkLabel (caseId ++ "_next" ++ show idx)
      compBI <- cgMkConstInteger bi
      match <- icmp "eq" (Const I64 0) !(compareInteger compBI scrutinee)
      appendCode $ "br " ++ toIR match ++ ", " ++ toIR labelAltStart ++ ", " ++ toIR labelAltNext
      beginLabel labelAltStart
      traverse_ getInstIRWithComment is
      appendCode $ "br " ++ toIR labelEnd
      beginLabel labelAltNext
    makeCaseAlt _ _ _ (_, c, _) = appendCode $ "ERROR: constcase must be BI, got: " ++ show c

intBinary : (IRValue I64 -> IRValue I64 -> Codegen (IRValue I64)) -> Reg -> Reg -> Reg -> Codegen ()
intBinary op dest a b = do
  i1 <- unboxInt (reg2val a)
  i2 <- unboxInt (reg2val b)
  obj <- cgMkInt !(op i1 i2)
  store obj (reg2val dest)

bits64Binary : (IRValue I64 -> IRValue I64 -> Codegen (IRValue I64)) -> Reg -> Reg -> Reg -> Codegen ()
bits64Binary op dest a b = do
  i1 <- unboxBits64 !(load (reg2val a))
  i2 <- unboxBits64 !(load (reg2val b))
  obj <- cgMkBits64 !(op i1 i2)
  store obj (reg2val dest)

intMask : Int -> Integer
intMask 8 = 0xff
intMask 16 = 0xffff
intMask 32 = 0xffffffff
intMask _ = 0

Show IntKind where
  show (Signed Unlimited) = "(Signed Unlimited)"
  show (Signed (P x)) = "(Signed (P " ++ show x ++ "))"
  show (Unsigned x) = "(Unsigned " ++ show x ++ ")"

bits64Compare : String -> Reg -> Reg -> Reg -> Codegen ()
bits64Compare op dest a b = do
  i1 <- unboxBits64 !(load (reg2val a))
  i2 <- unboxBits64 !(load (reg2val b))
  result <- mkZext !(icmp op i1 i2)
  obj <- cgMkInt result
  store obj (reg2val dest)

typeFromBits : Int -> IRType
typeFromBits 8 = I8
typeFromBits 16 = I16
typeFromBits 32 = I32
typeFromBits _ = I64

intCompare : Int -> String -> Reg -> Reg -> Reg -> Codegen ()
intCompare bits op dest a b = do
  let t = typeFromBits bits
  i1 <- mkTrunc {to=t} !(unboxInt (reg2val a))
  i2 <- mkTrunc !(unboxInt (reg2val b))
  result <- mkZext !(icmp op i1 i2)
  obj <- cgMkInt result
  store obj (reg2val dest)

intCompare' : Maybe IntKind ->
              (unsignedOp : String) ->
              (signedOp : String) ->
              (dest : Reg) -> Reg -> Reg ->
              Codegen ()
intCompare' (Just (Unsigned bits)) unsignedOp _ dest a b = do
  if (bits == 64)
     then bits64Compare unsignedOp dest a b
     else intCompare bits unsignedOp dest a b
intCompare' (Just (Signed (P bits))) _ signedOp dest a b = do
  if (bits == 64)
     then bits64Compare signedOp dest a b
     else intCompare bits signedOp dest a b
intCompare' (Just k) _ _ _ _ _ = addError ("invalid IntKind for binary operator: " ++ show k)
intCompare' (Nothing) _ _ _ _ _ = addError ("binary operator used with no IntKind")

boundedIntBinary' : Maybe IntKind ->
                    (unsignedOp : IRValue I64 -> IRValue I64 -> Codegen (IRValue I64)) ->
                    (signedOp : IRValue I64 -> IRValue I64 -> Codegen (IRValue I64)) ->
                    (dest : Reg) -> Reg -> Reg ->
                    Codegen ()
boundedIntBinary' (Just (Unsigned bits)) unsignedOp _ dest a b = do
  let mask = intMask bits
  when (mask == 0) (addError "invalid bitMask for binary op")

  i1 <- unboxInt (reg2val a)
  i2 <- unboxInt (reg2val b)
  result <- unsignedOp i1 i2
  truncatedVal <- mkAnd (Const I64 mask) result
  obj <- cgMkInt truncatedVal
  store obj (reg2val dest)
boundedIntBinary' (Just (Signed (P bits))) _ signedOp dest a b = do
  let mask = intMask bits
  when (mask == 0) (addError "invalid bitMask for binary op")
  i1 <- unboxIntSigned bits (reg2val a)
  i2 <- unboxIntSigned bits (reg2val b)
  result <- signedOp i1 i2
  truncatedVal <- mkAnd (Const I64 mask) result
  obj <- cgMkInt truncatedVal
  store obj (reg2val dest)
boundedIntBinary' (Just k) _ _ _ _ _ = addError ("invalid IntKind for binary operator: " ++ show k)
boundedIntBinary' (Nothing) _ _ _ _ _ = addError ("binary operator used with no IntKind")

objBinary : (op : IRValue IRObjPtr -> IRValue IRObjPtr -> Codegen (IRValue IRObjPtr)) ->
            (dest : Reg) -> Reg -> Reg ->
            Codegen ()
objBinary op dest r1 r2 = do
  o1 <- load (reg2val r1)
  o2 <- load (reg2val r2)
  newObj <- op o1 o2
  store newObj (reg2val dest)

genericIntUnbox : Constant -> IRValue IRObjPtr -> Codegen (IRValue I64)
genericIntUnbox IntegerType obj = unboxIntegerSigned obj
genericIntUnbox IntType obj = unboxInt' obj
genericIntUnbox ty obj with (intKind ty)
  genericIntUnbox _ obj | Just (Unsigned 64) = unboxBits64 obj
  genericIntUnbox _ obj | Just (Unsigned bits) = unboxInt' obj
  genericIntUnbox _ obj | Just (Signed (P 64)) = unboxBits64 obj
  genericIntUnbox _ obj | Just (Signed (P bits)) = unboxIntSigned' bits obj
  genericIntUnbox ty _ | _ = do
    addError ("invalid int unbox: " ++ show ty)
    pure (Const I64 0)

genericIntBox : Constant -> IRValue I64 -> Codegen (IRValue IRObjPtr)
genericIntBox IntegerType ival = cgMkIntegerSigned ival
genericIntBox IntType ival = cgMkInt ival
genericIntBox ty ival with (intKind ty)
  genericIntBox _ ival | Just (Unsigned 64) = cgMkBits64 ival
  genericIntBox _ ival | Just (Unsigned bits) = do
    let mask = intMask bits
    truncatedVal <- mkAnd (Const I64 mask) ival
    cgMkInt truncatedVal
  genericIntBox _ ival | Just (Signed (P 64)) = cgMkBits64 ival
  genericIntBox _ ival | Just (Signed (P bits)) = do
    let mask = intMask bits
    truncatedVal <- mkAnd (Const I64 mask) ival
    cgMkInt truncatedVal
  genericIntBox ty _ | _ = do
    addError ("invalid int box: " ++ show ty)
    cgMkInt (Const I64 0)

castError : Constant -> Constant -> Codegen (IRValue IRObjPtr)
castError fromType toType = do
  addError ("cast not implemented: " ++ (show fromType) ++ " -> " ++ (show toType))
  pure nullPtr

genericCastFromDouble : (toType : Constant) -> IRValue IRObjPtr -> Codegen (IRValue IRObjPtr)
genericCastFromDouble toType src = do
  f1 <- unboxDouble src
  case (intKind toType) of
       Just (Unsigned _) => genericIntBox toType !(fptoui f1)
       Just (Signed (P _)) => genericIntBox toType !(fptosi f1)
       Just (Signed Unlimited) => castDoubleToInteger src
       _ => castError DoubleType toType

genericCast : Constant -> Constant -> IRValue IRObjPtr -> Codegen (IRValue IRObjPtr)
-- to Char
genericCast fromType CharType src =
  case (intKind fromType) of
       Just _ => do
         raw <- genericIntUnbox fromType src
         ival <- mkTrunc {to=I32} raw
         -- this also helps to rule out negative values, should fromType be signed
         surrogateUpperBound <- icmp "ult" ival (Const I32 0xe000)
         surrogateLowerBound <- icmp "ugt" ival (Const I32 0xd7ff)
         isSurrogate <- mkAnd surrogateLowerBound surrogateUpperBound
         tooHigh <- icmp "ugt" ival (Const I32 0x10ffff)
         isInvalid <- mkOr tooHigh isSurrogate
         codepoint <- mkSelect isInvalid (Const I32 0) ival
         cgMkChar codepoint
       Nothing => castError fromType CharType

-- to Double
genericCast fromType DoubleType src = do
  ival <- genericIntUnbox fromType src
  case (intKind fromType) of
       Just (Unsigned _) => cgMkDouble !(uitofp ival)
       Just (Signed Unlimited) => castIntegerToDouble src
       Just (Signed (P _)) => cgMkDouble !(sitofp ival)
       Nothing => castError fromType DoubleType

-- from Double
genericCast DoubleType toType src = genericCastFromDouble toType src

-- to String
genericCast IntegerType StringType src = castIntegerToString src
genericCast fromType StringType src =
  case (intKind fromType) of
       Just _ => do
         ival <- genericIntUnbox fromType src
         -- max size of 2^64 = 20 + (optional "-" prefix) + NUL byte (from snprintf)
         newStr <- dynamicAllocate (ConstI64 24)
         strPayload <- getObjectPayloadAddr {t=I8} newStr
         length <- (SSA I64) <$> assignSSA ("call ccc i64 @idris_rts_int_to_str(" ++ toIR strPayload ++ ", " ++ toIR ival ++ ")")
         newHeader <- mkHeader OBJECT_TYPE_ID_STR !(mkTrunc length)
         putObjectHeader newStr newHeader
         pure newStr
       Nothing => castError fromType StringType

-- from String
genericCast StringType IntegerType src = castStringToInteger src
genericCast StringType toType src =
  case (intKind toType) of
       Just _ => do
         parsedVal <- SSA I64 <$> assignSSA ("  call ccc i64 @idris_rts_str_to_int(" ++ toIR src ++ ")")
         genericIntBox toType parsedVal
       Nothing => castError StringType toType

-- from generic int to Int
genericCast fromType IntType src =
  case (intKind fromType) of
       Just _ => do
         ival <- genericIntUnbox fromType src
         cgMkInt ival
       Nothing => castError fromType IntType

genericCast Bits64Type IntegerType src =
  cgMkIntegerUnsigned !(unboxBits64 src)

-- from generic int to generic int
genericCast fromType toType src = do
  case (intKind fromType) of
       Just _ => do
         ival <- genericIntUnbox fromType src
         case (intKind toType) of
              Just (Unsigned 64) => cgMkBits64 ival
              Just (Unsigned bits) => do
                let mask = intMask bits
                truncatedVal <- mkAnd (Const I64 mask) ival
                cgMkInt truncatedVal
              Just (Signed (P 64)) => cgMkBits64 ival
              Just (Signed (P bits)) => do
                let mask = intMask bits
                truncatedVal <- mkAnd (Const I64 mask) ival
                cgMkInt truncatedVal
              Just (Signed Unlimited) => do
                cgMkIntegerSigned ival
              Nothing => castError fromType toType
       Nothing => castError fromType toType

getInstForConstCaseIntLike : {auto conNames : SortedMap Name Int} -> Constant -> Reg -> List (Constant, List VMInst) -> Maybe (List VMInst) -> Codegen ()
getInstForConstCaseIntLike ty r alts def =
  do caseId <- mkVarName "case_"
     let def' = fromMaybe [(ERROR $ "no default in const case (int)" ++ caseId)] def
     let labelEnd = caseId ++ "_end"
     scrutinee <- genericIntUnbox ty !(load $ reg2val r)
     appendCode $ "  switch " ++ toIR scrutinee ++ ", label %" ++ caseId ++ "_default [ " ++ (showSep "\n      " (map (makeConstCaseLabel caseId) alts)) ++ " ]"
     appendCode $ caseId ++ "_default:"
     traverse_ getInstIRWithComment def'
     appendCode $ "br label %" ++ labelEnd
     traverse_ (makeCaseAlt caseId) alts
     appendCode $ labelEnd ++ ":"
     pure ()
  where
    makeCaseAlt : String -> (Constant, List VMInst) -> Codegen ()
    makeCaseAlt caseId (c, is) = do
      appendCode $ makeConstCaseLabelName caseId c ++ ":"
      traverse_ getInstIRWithComment is
      appendCode $ "br label %" ++ caseId ++ "_end"

integerCmp : String -> Reg -> Reg -> Reg -> Codegen ()
integerCmp op dest a b = do
  intObj1 <- load (reg2val a)
  intObj2 <- load (reg2val b)
  cmpRaw <- compareInteger intObj1 intObj2
  cmpResult <- icmp op cmpRaw (Const I64 0)
  obj <- cgMkInt !(mkZext {to=I64} cmpResult)
  store obj (reg2val dest)

stringCmp : CompareOp -> Reg -> Reg -> Reg -> Codegen ()
stringCmp op dest r1 r2 = do
  o1 <- load (reg2val r1)
  o2 <- load (reg2val r2)
  result <- stringCompare op o1 o2
  store result (reg2val dest)

doubleCmp : String -> Reg -> Reg -> Reg -> Codegen ()
doubleCmp op dest a b = do
  f1 <- unboxDouble !(load $ reg2val a)
  f2 <- unboxDouble !(load $ reg2val b)
  compared <- fcmp op f1 f2
  store !(cgMkInt !(mkZext compared)) (reg2val dest)

doubleBinOp : String -> Reg -> Reg -> Reg -> Codegen ()
doubleBinOp op dest a b = do
  f1 <- unboxDouble !(load $ reg2val a)
  f2 <- unboxDouble !(load $ reg2val b)
  result <- SSA F64 <$> (assignSSA $ op ++ " double " ++ showWithoutType f1 ++ ", " ++ showWithoutType f2)
  store !(cgMkDouble result) (reg2val dest)

doubleUnaryFn : String -> Reg -> Reg -> Codegen ()
doubleUnaryFn funcName dest a = do
  val <- unboxDouble !(load $ reg2val a)
  result <- call {t=F64} "ccc" ("@" ++ funcName) [toIR val]
  store !(cgMkDouble result) (reg2val dest)

getInstIR : {auto conNames : SortedMap Name Int} -> VMInst -> Codegen ()
getInstIR (DECLARE (Loc r)) = do
  appendCode $ "  %v" ++ show r ++ "Var = alloca %ObjPtr"
  appendCode $ "  store %ObjPtr null, %ObjPtr* %v" ++ show r ++ "Var"
getInstIR (ASSIGN r src) = store !(load (reg2val src)) (reg2val r)

getInstIR (OP r Crash [r1, r2]) = do
  msg <- load (reg2val r2)
  appendCode $ "  call ccc void @idris_rts_crash_msg(" ++ toIR msg ++ ") noreturn"
  appendCode $ "unreachable"
getInstIR (ERROR s) = mkRuntimeCrash s
getInstIR (OP r BelieveMe [_, _, v]) = do
  store !(load (reg2val v)) (reg2val r)

getInstIR (OP r StrHead [r1]) = do
  assertObjectType r1 OBJECT_TYPE_ID_STR
  o1 <- load (reg2val r1)
  store !(stringHead o1) (reg2val r)

getInstIR (OP r StrTail [r1]) = do
  assertObjectType r1 OBJECT_TYPE_ID_STR
  o1 <- load (reg2val r1)
  store !(stringTail o1) (reg2val r)

getInstIR (OP r StrSubstr [r1, r2, r3]) = do
  assertObjectType r1 OBJECT_TYPE_ID_INT
  assertObjectType r2 OBJECT_TYPE_ID_INT
  assertObjectType r3 OBJECT_TYPE_ID_STR
  o1 <- load (reg2val r3)
  offset <- unboxInt (reg2val r1)
  length <- unboxInt (reg2val r2)
  subStr <- mkSubstring o1 offset length
  store subStr (reg2val r)

getInstIR (OP r StrAppend [r1, r2]) = do
  assertObjectType r1 OBJECT_TYPE_ID_STR
  assertObjectType r2 OBJECT_TYPE_ID_STR
  o1 <- load (reg2val r1)
  o2 <- load (reg2val r2)
  newStr <- stringAppend o1 o2
  store newStr (reg2val r)

getInstIR (OP r StrReverse [r1]) = do
  assertObjectType r1 OBJECT_TYPE_ID_STR
  strObj <- load (reg2val r1)
  store !(stringReverse strObj) (reg2val r)

getInstIR (OP r StrCons [r1, r2]) = do
  assertObjectType r1 OBJECT_TYPE_ID_CHAR
  assertObjectType r2 OBJECT_TYPE_ID_STR
  o1 <- load (reg2val r1)
  o2 <- load (reg2val r2)
  store !(stringCons o1 o2) (reg2val r)

getInstIR (OP r StrLength [r1]) = do
  assertObjectType r1 OBJECT_TYPE_ID_STR
  strObj <- load (reg2val r1)
  codepointCount <- getStringLength strObj
  sizeIntObj <- cgMkInt !(mkZext codepointCount)
  store sizeIntObj (reg2val r)
getInstIR (OP r StrIndex [r1, r2]) = do
  assertObjectType r1 OBJECT_TYPE_ID_STR
  assertObjectType r2 OBJECT_TYPE_ID_INT
  o1 <- load (reg2val r1)
  payload0 <- getObjectPayloadAddr {t=I8} o1
  strLenBytes <- getStringByteLength o1

  index <- mkTrunc {to=I32} !(unboxInt (reg2val r2))
  payload <- call {t=Pointer 1 I8} "ccc" "@utf8_codepoints_to_bytes" [toIR payload0, toIR index, toIR strLenBytes]
  -- TODO: bounds check
  charVal <- call {t=I32} "ccc" "@utf8_decode1" [toIR payload]

  newCharObj <- cgMkChar charVal
  store newCharObj (reg2val r)

getInstIR (OP r (LT  StringType) [r1, r2]) = stringCmp LT  r r1 r2
getInstIR (OP r (LTE StringType) [r1, r2]) = stringCmp LTE r r1 r2
getInstIR (OP r (EQ  StringType) [r1, r2]) = stringCmp EQ  r r1 r2
getInstIR (OP r (GTE StringType) [r1, r2]) = stringCmp GTE r r1 r2
getInstIR (OP r (GT  StringType) [r1, r2]) = stringCmp GT  r r1 r2

getInstIR (OP r (Cast Bits64Type StringType) [r1]) = do
  obj <- load (reg2val r1)
  theBits <- unboxBits64 obj

  -- max size of 2^64 = 20 + NUL byte (from snprintf)
  newStr <- dynamicAllocate (ConstI64 24)
  strPayload <- getObjectPayloadAddr {t=I8} newStr
  length <- (SSA I64) <$> assignSSA ("call ccc i64 @idris_rts_bits64_to_str(" ++ toIR strPayload ++ ", " ++ toIR theBits ++ ")")
  newHeader <- mkHeader OBJECT_TYPE_ID_STR !(mkTrunc length)
  putObjectHeader newStr newHeader
  store newStr (reg2val r)
getInstIR (OP r (Cast DoubleType StringType) [r1]) = do
  obj <- load (reg2val r1)
  theDouble <- getObjectSlot {t=F64} obj 0

  -- call once with nullptr as dest, to get required length
  length <- (SSA I64) <$> assignSSA ("call ccc i64 @idris_rts_double_to_str(%i8p1 null, i64 0, " ++ toIR theDouble ++ ")")
  -- snprintf writes an additional NUL byte to terminate the cstr
  lengthPlus1 <- mkAddNoWrap length (ConstI64 1)

  newStr <- dynamicAllocate lengthPlus1
  strPayload <- getObjectPayloadAddr {t=I8} newStr
  length <- (SSA I64) <$> assignSSA ("call ccc i64 @idris_rts_double_to_str(" ++ toIR strPayload ++ ", " ++ toIR lengthPlus1 ++ ", " ++ toIR theDouble ++ ")")
  newHeader <- mkHeader OBJECT_TYPE_ID_STR !(mkTrunc length)
  putObjectHeader newStr newHeader
  store newStr (reg2val r)

getInstIR (OP r (Cast StringType DoubleType) [r1]) = do
  strObj <- load (reg2val r1)
  parsedVal <- SSA F64 <$> assignSSA ("  call ccc double @idris_rts_str_to_double(" ++ toIR strObj ++ ")")
  newDouble <- cgMkDouble parsedVal
  store newDouble (reg2val r)

getInstIR (OP r (Cast CharType StringType) [r1]) = do
  o1 <- load (reg2val r1)
  charVal <- unboxChar' o1
  -- maximum length of one codepoint in UTF-8 is 4 bytes
  newStr <- dynamicAllocate (Const I64 4)
  newStrPayload1 <- getObjectPayloadAddr {t=I8} newStr
  charLength <- call {t=I32} "ccc" "@utf8_encode1" [toIR newStrPayload1, toIR charVal]
  putObjectHeader newStr !(mkHeader OBJECT_TYPE_ID_STR charLength)
  store newStr (reg2val r)

getInstIR (OP r (Cast CharType toType) [r1]) = do
  charVal <- unboxChar' !(load (reg2val r1))
  newInt <- genericIntBox toType !(mkZext charVal)
  store newInt (reg2val r)

getInstIR (OP r (Add Bits64Type) [r1, r2]) = bits64Binary mkAdd r r1 r2
getInstIR (OP r (Sub Bits64Type) [r1, r2]) = bits64Binary mkSub r r1 r2
getInstIR (OP r (Mul Bits64Type) [r1, r2]) = bits64Binary mkMul r r1 r2
getInstIR (OP r (Div Bits64Type) [r1, r2]) = bits64Binary mkUDiv r r1 r2
getInstIR (OP r (Mod Bits64Type) [r1, r2]) = bits64Binary mkURem r r1 r2
getInstIR (OP r (BAnd Bits64Type) [r1, r2]) = bits64Binary mkAnd r r1 r2
getInstIR (OP r (BOr Bits64Type) [r1, r2]) = bits64Binary mkOr r r1 r2
getInstIR (OP r (BXOr Bits64Type) [r1, r2]) = bits64Binary mkXOr r r1 r2
getInstIR (OP r (ShiftL Bits64Type) [r1, r2]) = bits64Binary mkShiftL r r1 r2
getInstIR (OP r (ShiftR Bits64Type) [r1, r2]) = bits64Binary mkShiftR r r1 r2

getInstIR (OP r (Add Int64Type) [r1, r2]) = bits64Binary mkAdd r r1 r2
getInstIR (OP r (Sub Int64Type) [r1, r2]) = bits64Binary mkSub r r1 r2
getInstIR (OP r (Mul Int64Type) [r1, r2]) = bits64Binary mkMul r r1 r2
getInstIR (OP r (Div Int64Type) [r1, r2]) = bits64Binary mkSDiv r r1 r2
getInstIR (OP r (Mod Int64Type) [r1, r2]) = bits64Binary mkSRem r r1 r2
getInstIR (OP r (BAnd Int64Type) [r1, r2]) = bits64Binary mkAnd r r1 r2
getInstIR (OP r (BOr Int64Type) [r1, r2]) = bits64Binary mkOr r r1 r2
getInstIR (OP r (BXOr Int64Type) [r1, r2]) = bits64Binary mkXOr r r1 r2
getInstIR (OP r (ShiftL Int64Type) [r1, r2]) = bits64Binary mkShiftL r r1 r2
getInstIR (OP r (ShiftR Int64Type) [r1, r2]) = bits64Binary mkAShiftR r r1 r2

getInstIR (OP r (Add IntType) [r1, r2]) = intBinary mkAdd r r1 r2
getInstIR (OP r (Sub IntType) [r1, r2]) = intBinary mkSub r r1 r2
getInstIR (OP r (Mul IntType) [r1, r2]) = intBinary mkMul r r1 r2
getInstIR (OP r (Div IntType) [r1, r2]) = intBinary mkSDiv r r1 r2
getInstIR (OP r (Mod IntType) [r1, r2]) = intBinary mkSRem r r1 r2
getInstIR (OP r (BAnd IntType) [r1, r2]) = intBinary mkAnd r r1 r2
getInstIR (OP r (BOr IntType) [r1, r2]) = intBinary mkOr r r1 r2
getInstIR (OP r (BXOr IntType) [r1, r2]) = intBinary mkXOr r r1 r2
getInstIR (OP r (ShiftL IntType) [r1, r2]) = intBinary mkShiftL r r1 r2
getInstIR (OP r (ShiftR IntType) [r1, r2]) = intBinary mkShiftR r r1 r2

getInstIR (OP r (Add IntegerType) [r1, r2]) = do
  i1 <- load (reg2val r1)
  i2 <- load (reg2val r2)

  s1 <- getObjectSize i1
  s2 <- getObjectSize i2
  sx <- mkXOr s1 s2
  signsMatch <- icmp "sge" sx (Const I32 0)
  obj <- mkIf (pure signsMatch) (addInteger i1 i2) (subInteger i1 i2)
  store obj (reg2val r)
getInstIR (OP r (Sub IntegerType) [r1, r2]) = do
  i1 <- load (reg2val r1)
  i2 <- load (reg2val r2)

  s1 <- getObjectSize i1
  s2 <- getObjectSize i2
  sx <- mkXOr s1 s2
  signsMatch <- icmp "sge" sx (Const I32 0)
  obj <- mkIf (pure signsMatch) (subInteger i1 i2) (addInteger i1 i2)
  store obj (reg2val r)
getInstIR (OP r (Mul IntegerType) [r1, r2]) = objBinary mulInteger r r1 r2
getInstIR (OP r (Div IntegerType) [r1, r2]) = objBinary divIntegerQuotient r r1 r2
getInstIR (OP r (Mod IntegerType) [r1, r2]) = objBinary divIntegerRemainder r r1 r2
getInstIR (OP r (ShiftL IntegerType) [r1, r2]) = objBinary shiftLeftInteger r r1 r2
getInstIR (OP r (ShiftR IntegerType) [r1, r2]) = objBinary shiftRightInteger r r1 r2
getInstIR (OP r (BAnd IntegerType) [r1, r2]) = objBinary andInteger r r1 r2
getInstIR (OP r (BOr IntegerType) [r1, r2]) = objBinary orInteger r r1 r2
getInstIR (OP r (BXOr IntegerType) [r1, r2]) = objBinary xorInteger r r1 r2

getInstIR (OP r (LT CharType) [r1, r2]) = do
  -- compare Chars by comparing their headers
  o1 <- load (reg2val r1)
  o2 <- load (reg2val r2)
  i1 <- getObjectHeader o1
  i2 <- getObjectHeader o2
  cmp_i1 <- icmp "ult" i1 i2
  cmp_i64 <- assignSSA $ "zext " ++ toIR cmp_i1 ++ " to i64"
  obj <- cgMkInt (SSA I64 cmp_i64)
  store obj (reg2val r)
getInstIR (OP r (LTE CharType) [r1, r2]) = do
  -- compare Chars by comparing their headers
  o1 <- load (reg2val r1)
  o2 <- load (reg2val r2)
  i1 <- getObjectHeader o1
  i2 <- getObjectHeader o2
  cmp_i1 <- icmp "ule" i1 i2
  cmp_i64 <- assignSSA $ "zext " ++ toIR cmp_i1 ++ " to i64"
  obj <- cgMkInt (SSA I64 cmp_i64)
  store obj (reg2val r)
getInstIR (OP r (GTE CharType) [r1, r2]) = do
  -- compare Chars by comparing their headers
  o1 <- load (reg2val r1)
  o2 <- load (reg2val r2)
  i1 <- getObjectHeader o1
  i2 <- getObjectHeader o2
  cmp_i1 <- icmp "uge" i1 i2
  cmp_i64 <- assignSSA $ "zext " ++ toIR cmp_i1 ++ " to i64"
  obj <- cgMkInt (SSA I64 cmp_i64)
  store obj (reg2val r)
getInstIR (OP r (GT CharType) [r1, r2]) = do
  -- compare Chars by comparing their headers
  o1 <- load (reg2val r1)
  o2 <- load (reg2val r2)
  i1 <- getObjectHeader o1
  i2 <- getObjectHeader o2
  cmp_i1 <- icmp "ugt" i1 i2
  cmp_i64 <- assignSSA $ "zext " ++ toIR cmp_i1 ++ " to i64"
  obj <- cgMkInt (SSA I64 cmp_i64)
  store obj (reg2val r)
getInstIR (OP r (EQ CharType) [r1, r2]) = do
  -- Two Chars are equal, iff their headers are equal
  o1 <- load (reg2val r1)
  o2 <- load (reg2val r2)
  i1 <- getObjectHeader o1
  i2 <- getObjectHeader o2
  cmp_i1 <- icmp "eq" i1 i2
  cmp_i64 <- assignSSA $ "zext " ++ toIR cmp_i1 ++ " to i64"
  obj <- cgMkInt (SSA I64 cmp_i64)
  store obj (reg2val r)

getInstIR (OP r (LT Bits8Type) [r1, r2]) = do
  i1 <- unboxInt (reg2val r1)
  i2 <- unboxInt (reg2val r2)
  vsum_i1 <- icmp "ult" i1 i2
  vsum_i64 <- mkZext {to=I64} vsum_i1
  obj <- cgMkInt vsum_i64
  store obj (reg2val r)

getInstIR (OP r (LT IntType) [r1, r2]) = do
  i1 <- unboxInt (reg2val r1)
  i2 <- unboxInt (reg2val r2)
  vsum_i1 <- icmp "slt" i1 i2
  vsum_i64 <- mkZext {to=I64} vsum_i1
  obj <- cgMkInt vsum_i64
  store obj (reg2val r)
getInstIR (OP r (LTE IntType) [r1, r2]) = do
  i1 <- unboxInt (reg2val r1)
  i2 <- unboxInt (reg2val r2)
  vsum_i1 <- icmp "sle" i1 i2
  vsum_i64 <- mkZext {to=I64} vsum_i1
  obj <- cgMkInt vsum_i64
  store obj (reg2val r)
getInstIR (OP r (EQ IntType) [r1, r2]) = do
  i1 <- unboxInt (reg2val r1)
  i2 <- unboxInt (reg2val r2)
  vsum_i1 <- icmp "eq" i1 i2
  vsum_i64 <- mkZext {to=I64} vsum_i1
  obj <- cgMkInt vsum_i64
  store obj (reg2val r)
getInstIR (OP r (GTE IntType) [r1, r2]) = do
  i1 <- unboxInt (reg2val r1)
  i2 <- unboxInt (reg2val r2)
  vsum_i1 <- icmp "sge" i1 i2
  vsum_i64 <- mkZext {to=I64} vsum_i1
  obj <- cgMkInt vsum_i64
  store obj (reg2val r)
getInstIR (OP r (GT IntType) [r1, r2]) = do
  i1 <- unboxInt (reg2val r1)
  i2 <- unboxInt (reg2val r2)
  vsum_i1 <- icmp "sgt" i1 i2
  vsum_i64 <- mkZext {to=I64} vsum_i1
  obj <- cgMkInt vsum_i64
  store obj (reg2val r)

getInstIR (OP r (LT  IntegerType) [r1, r2]) = integerCmp "slt" r r1 r2
getInstIR (OP r (LTE IntegerType) [r1, r2]) = integerCmp "sle" r r1 r2
getInstIR (OP r (EQ  IntegerType) [r1, r2]) = integerCmp "eq"  r r1 r2
getInstIR (OP r (GTE IntegerType) [r1, r2]) = integerCmp "sge" r r1 r2
getInstIR (OP r (GT  IntegerType) [r1, r2]) = integerCmp "sgt" r r1 r2

getInstIR (OP r (Cast fromType toType) [r1]) = do
  castedObj <- genericCast fromType toType !(load (reg2val r1))
  store castedObj (reg2val r)

getInstIR (OP r (LT  DoubleType) [r1, r2]) = doubleCmp "olt" r r1 r2
getInstIR (OP r (LTE DoubleType) [r1, r2]) = doubleCmp "ole" r r1 r2
getInstIR (OP r (EQ  DoubleType) [r1, r2]) = doubleCmp "oeq" r r1 r2
getInstIR (OP r (GTE DoubleType) [r1, r2]) = doubleCmp "oge" r r1 r2
getInstIR (OP r (GT  DoubleType) [r1, r2]) = doubleCmp "ogt" r r1 r2

getInstIR (OP r (Add DoubleType) [r1, r2]) = doubleBinOp "fadd" r r1 r2
getInstIR (OP r (Sub DoubleType) [r1, r2]) = doubleBinOp "fsub" r r1 r2
getInstIR (OP r (Mul DoubleType) [r1, r2]) = doubleBinOp "fmul" r r1 r2
getInstIR (OP r (Div DoubleType) [r1, r2]) = doubleBinOp "fdiv" r r1 r2
getInstIR (OP r (Mod DoubleType) [r1, r2]) = doubleBinOp "frem" r r1 r2
getInstIR (OP r (Neg DoubleType) [r1]) = do
  fv <- unboxFloat64 (reg2val r1)
  neg <- (SSA F64) <$> assignSSA ("fneg " ++ toIR fv)
  obj <- cgMkDouble neg
  store obj (reg2val r)

getInstIR (OP r DoubleExp [r1]) = doubleUnaryFn "llvm.exp.f64" r r1
getInstIR (OP r DoubleLog [r1]) = doubleUnaryFn "llvm.log.f64" r r1
getInstIR (OP r DoubleSin [r1]) = doubleUnaryFn "llvm.sin.f64" r r1
getInstIR (OP r DoubleCos [r1]) = doubleUnaryFn "llvm.cos.f64" r r1
getInstIR (OP r DoubleTan [r1]) = doubleUnaryFn "tan" r r1
getInstIR (OP r DoubleASin [r1]) = doubleUnaryFn "asin" r r1
getInstIR (OP r DoubleACos [r1]) = doubleUnaryFn "acos" r r1
getInstIR (OP r DoubleATan [r1]) = doubleUnaryFn "atan" r r1
getInstIR (OP r DoubleSqrt [r1]) = doubleUnaryFn "llvm.sqrt.f64" r r1
getInstIR (OP r DoubleFloor [r1]) = doubleUnaryFn "llvm.floor.f64" r r1
getInstIR (OP r DoubleCeiling [r1]) = doubleUnaryFn "llvm.ceil.f64" r r1

getInstIR (OP r (Add ty) [r1, r2]) = boundedIntBinary' (intKind ty) mkAddNoWrap mkAddNoWrap r r1 r2
getInstIR (OP r (Sub ty) [r1, r2]) = boundedIntBinary' (intKind ty) mkSub mkSub r r1 r2
getInstIR (OP r (Mul ty) [r1, r2]) = boundedIntBinary' (intKind ty) mkMul mkMul r r1 r2
getInstIR (OP r (Div ty) [r1, r2]) = boundedIntBinary' (intKind ty) mkUDiv mkSDiv r r1 r2
getInstIR (OP r (Mod ty) [r1, r2]) = boundedIntBinary' (intKind ty) mkURem mkSRem r r1 r2
getInstIR (OP r (BAnd ty) [r1, r2]) = boundedIntBinary' (intKind ty) mkAnd mkAnd r r1 r2
getInstIR (OP r (BOr ty) [r1, r2]) = boundedIntBinary' (intKind ty) mkOr mkOr r r1 r2
getInstIR (OP r (BXOr ty) [r1, r2]) = boundedIntBinary' (intKind ty) mkXOr mkXOr r r1 r2
getInstIR (OP r (ShiftL ty) [r1, r2]) = boundedIntBinary' (intKind ty) mkShiftL mkShiftL r r1 r2
getInstIR (OP r (ShiftR ty) [r1, r2]) = boundedIntBinary' (intKind ty) mkShiftR mkAShiftR r r1 r2

getInstIR (OP r (LT  ty) [r1, r2]) = intCompare' (intKind ty) "ult" "slt" r r1 r2
getInstIR (OP r (LTE ty) [r1, r2]) = intCompare' (intKind ty) "ule" "sle" r r1 r2
getInstIR (OP r (EQ  ty) [r1, r2]) = intCompare' (intKind ty) "eq"  "eq"  r r1 r2
getInstIR (OP r (GTE ty) [r1, r2]) = intCompare' (intKind ty) "uge" "sge" r r1 r2
getInstIR (OP r (GT  ty) [r1, r2]) = intCompare' (intKind ty) "ugt" "sgt" r r1 r2

getInstIR (MKCON r (Left tag) args) = do
  obj <- mkCon tag !(traverse prepareArg args)
  store obj (reg2val r)
getInstIR {conNames} (MKCON r (Right n) args) = do
  case lookup n conNames of
       Just nameId => do
         loadedArgs <- traverse prepareArg args
         obj <- mkCon (makeNameId nameId) loadedArgs
         store obj (reg2val r)
       Nothing => addError $ "MKCON name not found: " ++ show n

getInstIR (MKCLOSURE r n missingN args) = do
  argsV <- traverse prepareArg args
  newObj <- mkClosure n missingN argsV
  store newObj (reg2val r)

getInstIR (APPLY r fun arg) = do
  hp <- ((++) "%RuntimePtr ") <$> assignSSA "load %RuntimePtr, %RuntimePtr* %HpVar"
  hpLim <- ((++) "%RuntimePtr ") <$> assignSSA "load %RuntimePtr, %RuntimePtr* %HpLimVar"
  let base = "%TSOPtr %BaseArg"

  closureObj <- load (reg2val fun)
  argV <- load (reg2val arg)

  let tailpos = isReturn r
  let tailStr = if tailpos then "tail " else ""

  result <- assignSSA $ tailStr ++ "call fastcc %Return1 @idris_apply_closure(" ++ showSep ", " [hp, base, hpLim, toIR closureObj, toIR argV] ++ ")"

  when tailpos $ appendCode $ "ret %Return1 " ++ result
  when tailpos $appendCode $ "unreachable"

  newHp <- assignSSA $ "extractvalue %Return1 " ++ result ++ ", 0"
  appendCode $ "store %RuntimePtr " ++ newHp ++ ", %RuntimePtr* %HpVar"
  newHpLim <- assignSSA $ "extractvalue %Return1 " ++ result ++ ", 1"
  appendCode $ "store %RuntimePtr " ++ newHpLim ++ ", %RuntimePtr* %HpLimVar"
  returnValue <- assignSSA $ "extractvalue %Return1 " ++ result ++ ", 2"
  appendCode $ "store %ObjPtr " ++ returnValue ++ ", %ObjPtr* " ++ toIR r ++ "Var"

  pure ()

getInstIR (MKCONSTANT r (Ch c)) = do
  newObj <- cgMkChar (Const I32 $ cast c)
  store newObj (reg2val r)
getInstIR (MKCONSTANT r (B8 c)) = do
  obj <- cgMkInt (ConstI64 $ cast c)
  store obj (reg2val r)
getInstIR (MKCONSTANT r (B16 c)) = do
  obj <- cgMkInt (ConstI64 $ cast c)
  store obj (reg2val r)
getInstIR (MKCONSTANT r (B32 c)) = do
  obj <- cgMkInt (ConstI64 $ cast c)
  store obj (reg2val r)
getInstIR (MKCONSTANT r (B64 c)) = do
  obj <- cgMkBits64 (ConstI64 $ cast c)
  store obj (reg2val r)
getInstIR (MKCONSTANT r (I8 c)) = do
  obj <- cgMkInt (ConstI64 $ cast c)
  store obj (reg2val r)
getInstIR (MKCONSTANT r (I16 c)) = do
  obj <- cgMkInt (ConstI64 $ cast c)
  store obj (reg2val r)
getInstIR (MKCONSTANT r (I32 c)) = do
  obj <- cgMkInt (ConstI64 $ cast c)
  store obj (reg2val r)
getInstIR (MKCONSTANT r (I64 c)) = do
  obj <- cgMkBits64 (ConstI64 $ cast c)
  store obj (reg2val r)
getInstIR (MKCONSTANT r (I c)) = do
  obj <- cgMkInt (ConstI64 $ (cast {to=Integer} c) `mod` 0x7fffffffffffffff)
  store obj (reg2val r)
getInstIR (MKCONSTANT r (BI c)) = do
  obj <- cgMkConstInteger c
  store obj (reg2val r)
getInstIR (MKCONSTANT r (Db d)) = do
  obj <- cgMkConstDouble d
  store obj (reg2val r)
getInstIR (MKCONSTANT r WorldVal) = do
  obj <- mkCon 1337 []
  store obj (reg2val r)
getInstIR (MKCONSTANT r (Str s)) = store !(mkStr s) (reg2val r)

getInstIR (CONSTCASE r alts def) = case findConstCaseType alts of
                                          Right (IntLikeCase ty) => getInstForConstCaseIntLike ty r alts def
                                          Right BigIntCase => getInstForConstCaseInteger r alts def
                                          Right StringCase => getInstForConstCaseString r alts def
                                          Right CharCase => getInstForConstCaseChar r alts def
                                          Left err => addError ("constcase error: " ++ err)

getInstIR {conNames} (CASE r alts def) =
  do let def' = fromMaybe [(ERROR $ "no default in CASE")] def
     --appendCode $ "call ccc i32 @dump_obj(" ++ toIR !(load $ reg2val r) ++ ") "
     assertObjectType r OBJECT_TYPE_ID_CON_NO_ARGS
     caseId <- mkVarName "case_"
     let labelEnd = caseId ++ "_end"
     o1 <- load $ reg2val r
     header <- getObjectHeader o1
     -- object tag is stored in the least significat 32 bits of header
     scrutinee <- assignSSA $ "and i64 " ++ (show 0xffffffff) ++ ", " ++ showWithoutType header
     appendCode $ "  switch i64 " ++ scrutinee ++ ", label %" ++ caseId ++ "_default [ " ++ (showSep "\n      " !(traverse (makeCaseLabel caseId) alts)) ++ " ]"
     appendCode $ caseId ++ "_default:"
     traverse_ getInstIRWithComment def'
     appendCode $ "br label %" ++ labelEnd
     traverse_ (makeCaseAlt caseId) alts
     appendCode $ labelEnd ++ ":"
     pure ()
  where
    makeCaseAlt : String -> (Either Int Name, List VMInst) -> Codegen ()
    makeCaseAlt caseId (Left c, is) = do
      appendCode $ caseId ++ "_tag_is_" ++ (show c) ++ ":"
      traverse_ getInstIRWithComment is
      appendCode $ "br label %" ++ caseId ++ "_end"
    makeCaseAlt caseId (Right n, is) =
      case lookup n conNames of
           Just nameId => do
             appendCode $ "; " ++ (show n) ++ " -> " ++ (show nameId)
             appendCode (caseId ++ "_name_is_" ++ (show (makeNameId nameId)) ++ ":")
             traverse_ getInstIRWithComment is
             appendCode ("br label %" ++ caseId ++ "_end")
           Nothing => addError $ "name for case not found: " ++ show n

getInstIR (CALL r tailpos n args) =
  do argsV <- traverse prepareArg args
     hp <- ((++) "%RuntimePtr ") <$> assignSSA "load %RuntimePtr, %RuntimePtr* %HpVar"
     hpLim <- ((++) "%RuntimePtr ") <$> assignSSA "load %RuntimePtr, %RuntimePtr* %HpLimVar"
     let base = "%TSOPtr %BaseArg"

     let tailStr = if tailpos then "tail " else ""
     result <- assignSSA $ tailStr ++ "call fastcc %Return1 @" ++ (safeName n) ++ "(" ++ showSep ", " (hp::base::hpLim::(map toIR argsV)) ++ ")"

     when tailpos $ appendCode $ "ret %Return1 " ++ result
     when tailpos $ appendCode $ "unreachable"

     newHp <- assignSSA $ "extractvalue %Return1 " ++ result ++ ", 0"
     appendCode $ "store %RuntimePtr " ++ newHp ++ ", %RuntimePtr* %HpVar"
     newHpLim <- assignSSA $ "extractvalue %Return1 " ++ result ++ ", 1"
     appendCode $ "store %RuntimePtr " ++ newHpLim ++ ", %RuntimePtr* %HpLimVar"
     returnValue <- assignSSA $ "extractvalue %Return1 " ++ result ++ ", 2"
     appendCode $ "store %ObjPtr " ++ returnValue ++ ", %ObjPtr* " ++ toIR r ++ "Var"
     pure ()

getInstIR (PROJECT r o pos) = do
  assertObjectType o OBJECT_TYPE_ID_CON_NO_ARGS
  obj <- load {t=IRObjPtr} (reg2val o)
  slot <- getObjectSlot {t=IRObjPtr} obj pos
  assertObjectTypeAny slot 0xf0
  store slot (reg2val r)

getInstIR (EXTPRIM r n args) = do
  loadedArgs <- traverse prepareArg args
  result <- compileExtPrim n loadedArgs
  store result (reg2val r)

getInstIR START = pure ()
getInstIR inst = do
  addError $ "NOT IMPLEMENTED: " ++ show inst
  mkRuntimeCrash ("NOT IMPLEMENTED: " ++ show inst)

getInstIRWithComment : {auto conNames : SortedMap Name Int} -> VMInst -> Codegen ()
getInstIRWithComment instr = do
  --appendCode (instrAsComment instr)
  getInstIR instr

getFunIR : SortedMap Name Int -> Name -> List Reg -> List VMInst -> Codegen ()
getFunIR conNames n args body = do
    fargs <- traverse argIR args
    debug <- debugEnabled <$> getOpts
    let visibility = if debug then "external" else "private"
    debugInfo <- if (not debug) then pure "" else do
      funTmd <- appendMetadata "!DISubroutineType(types: !{null, !0, !0, !0, !1})"
      funmd <- appendMetadata $ "distinct !DISubprogram(name: \"" ++ safeName n ++ "\", type: " ++ funTmd ++ ", unit: !99)"
      pure $ "!dbg " ++ funmd
    appendCode ("\n\ndefine " ++ visibility ++ " fastcc %Return1 @" ++ safeName n ++ "(" ++ (showSep ", " $ prepareArgCallConv fargs) ++ ") gc \"statepoint-example\" " ++ debugInfo ++ " {")
    appendCode "entry:"
    funcEntry
    traverse_ appendCode (map copyArg args)
    traverse_ getInstIRWithComment body
    funcReturn
    appendCode "}\n"

    let closureEntryName = if cast (length args) <= FAT_CLOSURE_LIMIT
                              then safeName n
                              else (safeName n) ++ "$$closureEntry"
    let closureEntryType = if cast (length args) <= FAT_CLOSURE_LIMIT
                              then "%FuncPtrArgs" ++ show (length args)
                              else "%FuncPtrClosureEntry"
    let closureHeader = constHeader OBJECT_TYPE_ID_CLOSURE (0x10000 * (cast $ length args))
    appendCode $ "@" ++ safeName n ++ "$$closureNoArgs = private unnamed_addr addrspace(1) constant {i64, %FuncPtr} {" ++ toIR closureHeader ++ ", %FuncPtr bitcast (\{closureEntryType} @\{closureEntryName} to %FuncPtr)}"
  where
    copyArg : Reg -> String
    copyArg (Loc i) = let r = show i in "  %v" ++ r ++ "Var = alloca %ObjPtr\n  store %ObjPtr %v" ++ r ++ ", %ObjPtr* %v" ++ r ++ "Var"
    copyArg _ = "ERROR: not an argument"

getFunIRClosureEntry : SortedMap Name Int -> Name -> (args : List Int) -> {auto ok : NonEmpty args} -> List VMInst -> Codegen ()
getFunIRClosureEntry conNames n args body = do
    debug <- debugEnabled <$> getOpts
    let visibility = if debug then "external" else "private"
    appendCode ("\n\ndefine " ++ visibility ++ " fastcc %Return1 @" ++ safeName n ++ "$$closureEntry(" ++ (showSep ", " $ prepareArgCallConv ["%ObjPtr %clObj", "%ObjPtr %lastArg"]) ++ ") gc \"statepoint-example\" {")
    appendCode "entry:"
    funcEntry
    traverse_ copyArg (enumerate $ init args)
    appendCode $ "  %v" ++ (show $ last args) ++ "Var = alloca %ObjPtr"
    store (SSA IRObjPtr "%lastArg") (reg2val $ Loc $ last args)
    traverse_ getInstIRWithComment body
    funcReturn
    appendCode "}\n"
  where
    copyArg : (Int, Int) -> Codegen ()
    copyArg (index, i) =
      let clObj = SSA IRObjPtr "%clObj" in do
        appendCode $ "  %v" ++ show i ++ "Var = alloca %ObjPtr"
        arg <- getObjectSlot clObj (index + 1)
        store arg (reg2val (Loc i))

getForeignFunctionIR : Name -> List String -> List CFType -> CFType -> Codegen ()
getForeignFunctionIR name cs args ret = do
  let found = findForeignName cs
  let builtin = found >>= ((flip lookup) builtinPrimitives)
  case (builtin, found) of
       (Just b, _) => builtinForeign b name args ret
       (Nothing, Just funcName) => genericForeign funcName name args ret
       (_, _) => missingForeign cs name args

export
getVMIR : CompileOpts -> SortedMap Name Int -> (Int, (Name, VMDef)) -> String
getVMIR opts conNames (i, n, MkVMFun args body) =
  let debug = debugEnabled opts in
  let closureEntry = if (cast $ length args) <= FAT_CLOSURE_LIMIT
                    then ""
                    else case args of
                              [] => ""
                              neArgs@(_::_) => runCodegen ({constNamespace := 2*i+1001} opts) $ getFunIRClosureEntry conNames n neArgs body
                              in
      (runCodegen ({constNamespace := (2*i+1000)} opts) $ getFunIR conNames n (map Loc args) body) ++ closureEntry
getVMIR opts conNames (i, (n, MkVMForeign cs args ret)) =
  let debug = debugEnabled opts in
      (runCodegen ({constNamespace := i} opts) $ getForeignFunctionIR n cs args ret) ++ "\n"
getVMIR _ _ (i, (n, MkVMError is)) = ""

funcPtrTypes : String
funcPtrTypes = fastConcat $ map funcPtr (rangeFromTo 0 FAT_CLOSURE_LIMIT) where
  funcPtr : Int -> String
  funcPtr i = "%FuncPtrArgs" ++ (show i) ++ " = type %Return1 (%RuntimePtr, %TSOPtr, %RuntimePtr" ++ repeatStr ", %ObjPtr" (integerToNat $ cast i) ++ ")*\n"

applyClosureHelperFunc : Codegen ()
applyClosureHelperFunc = do
  funcEntry

  let maxArgs = FAT_CLOSURE_LIMIT

  let closureObj = SSA IRObjPtr "%closureObjArg"
  let argValue = SSA IRObjPtr "%argumentObjArg"

  assertObjectType' closureObj OBJECT_TYPE_ID_CLOSURE

  closureHeader <- getObjectHeader closureObj
  argCount <- mkTrunc !(mkAnd (pConst 0xffff) closureHeader)
  missingArgCountShifted <- mkAnd (pConst 0xffff0000) closureHeader
  missingArgCount <- mkTrunc !(mkShiftR missingArgCountShifted (pConst 16))
  isSaturated <- icmp "eq" (pConst 1) missingArgCount
  lblSaturated <- genLabel "closure_saturated"
  lblUnsaturated <- genLabel "closure_unsaturated"
  branch isSaturated lblSaturated lblUnsaturated
  beginLabel lblSaturated

  funcPtrAdd <- getObjectSlotAddrVar {t=FuncPtr} closureObj (Const I64 0)
  funcPtr <- getObjectSlot {t=FuncPtr} closureObj 0

  let hp = "%RuntimePtr %HpArg"
  let base = "%TSOPtr %BaseArg"
  let hpLim = "%RuntimePtr %HpLimArg"

  lblApplyViaClosureEntry <- genLabel "apply_via_closure_entry"

  applyClosure <- mkVarName "apply_closure_"
  -- if the closure requires a total number of arguments <= FAT_CLOSURE_LIMIT
  -- (i.e. storedArgs <= (FAT_CLOSURE_LIMIT - 1)), it is invoked directly
  -- otherwise it is called via its "$$closureEntry" function
  appendCode $ "  switch " ++ toIR argCount ++ ", " ++ toIR lblApplyViaClosureEntry ++ " [\n  " ++
  (showSep "\n  " $ (flip map) (rangeFromTo 0 (maxArgs - 1)) (\i => "i32 " ++ show i ++ ", label %" ++ applyClosure ++ "_" ++ show i)) ++
  "]"

  for_ (rangeFromTo 0 (maxArgs - 1)) (\numberOfStoredArgs => do
    let labelName = applyClosure ++ "_" ++ show numberOfStoredArgs
    appendCode $ labelName ++ ":"
    storedArgs <- for (rangeFromThenTo 0 1 (numberOfStoredArgs-1)) (\argIndex => do
                      argItem <- getObjectSlot {t=IRObjPtr} closureObj (argIndex + 1)
                      pure $ (toIR argItem)
                      )
    let argList = [hp, base, hpLim] ++ storedArgs ++ [toIR argValue]
    func <- assignSSA $ "bitcast " ++ (toIR funcPtr) ++ " to %FuncPtrArgs" ++ show (numberOfStoredArgs+1)
    callRes <- assignSSA $ "tail call fastcc %Return1 " ++ func ++ "(" ++ (showSep ", " argList) ++ ")"
    appendCode $ "ret %Return1 " ++ callRes
    appendCode $ "unreachable"
    )

  beginLabel lblUnsaturated

  appliedArgCount <- mkAddNoWrap argCount (Const I32 1)
  newArgsSize <- mkMul appliedArgCount (Const I32 8)
  -- add 8 bytes for entry func ptr
  newPayloadSize <- mkAddNoWrap !(mkZext newArgsSize) (ConstI64 8)
  -- old payload size is new payload size - 8
  oldPayloadSize <- mkZext {to=I64} newArgsSize

  newClosure <- dynamicAllocate newPayloadSize

  newMissingArgs <- mkSub missingArgCount (Const I32 1)
  newMissingArgsShifted <- mkBinOp "shl" newMissingArgs (Const I32 16)
  newMissingAndAppliedArgs <- mkOr newMissingArgsShifted appliedArgCount
  newHeader <- mkHeader OBJECT_TYPE_ID_CLOSURE newMissingAndAppliedArgs

  oldPayloadPtr <- getObjectPayloadAddr {t=I8} closureObj
  newPayloadPtr <- getObjectPayloadAddr {t=I8} newClosure

  appendCode $ "  call void @llvm.memcpy.p1i8.p1i8.i64(" ++ toIR newPayloadPtr ++ ", " ++ toIR oldPayloadPtr ++ ", " ++ toIR oldPayloadSize ++ ", i1 false)"

  newArgSlotNumber <- mkZext appliedArgCount
  putObjectSlot newClosure newArgSlotNumber argValue

  putObjectHeader newClosure newHeader

  store newClosure (reg2val RVal)

  funcReturn

  beginLabel lblApplyViaClosureEntry
  closureEntryPtr <- assignSSA $ "bitcast " ++ (toIR funcPtr) ++ " to %FuncPtrClosureEntry"
  let argList = [hp, base, hpLim, toIR closureObj, toIR argValue]
  callRes <- assignSSA $ "tail call fastcc %Return1 " ++ closureEntryPtr ++ "(" ++ (showSep ", " argList) ++ ")"
  appendCode $ "ret %Return1 " ++ callRes
  appendCode $ "unreachable"

  appendCode $ "call ccc void @idris_rts_crash(i64 13)"
  appendCode "unreachable"

export
closureHelper : CompileOpts -> String
closureHelper opts = fastConcat [
  funcPtrTypes,
  "\ndefine fastcc %Return1 @idris_apply_closure(%RuntimePtr %HpArg, %TSOPtr %BaseArg, %RuntimePtr %HpLimArg, %ObjPtr %closureObjArg, %ObjPtr %argumentObjArg) gc \"statepoint-example\" {\n",
  runCodegen opts applyClosureHelperFunc,
  "\n}\n\n"
  ]
