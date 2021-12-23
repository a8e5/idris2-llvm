module Compiler.GenLLVMIR

import Data.Bits
import Data.Either
import Data.List
import Data.List1
import Data.Maybe
import Data.String
import Data.Vect
import System.Info

import Compiler.CompileExpr
import Compiler.VMCode
import Compiler.LLVM.IR
import Compiler.LLVM.Instruction
import Compiler.LLVM.Rapid.Builtin
import Compiler.LLVM.Rapid.Foreign
import Compiler.LLVM.Rapid.Object
import Control.Codegen
import Core.TT
import Data.Utils
import Libraries.Data.SortedMap
import Rapid.Common

-- work around Idris issue #2032: Slow typechecking on Int operation when Data.Fin.fromInteger is in scope
%hide Data.Fin.fromInteger

-- we provide our own in Data.Utils
%hide Core.Name.Namespace.showSep

CLOSURE_MAX_ARGS : Int
CLOSURE_MAX_ARGS = 1024

-- A "fat" closure is always invoked via its "closure entry" function
FAT_CLOSURE_LIMIT : Int
FAT_CLOSURE_LIMIT = 8

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

TARGET_SIZE_T : IRType
TARGET_SIZE_T = I64

MP_LIMB_T : IRType
MP_LIMB_T = I64

IEEE_DOUBLE_MASK_EXP  : Bits64
IEEE_DOUBLE_MASK_EXP  = 0x7ff0000000000000
IEEE_DOUBLE_MASK_FRAC : Bits64
IEEE_DOUBLE_MASK_FRAC = 0x000fffffffffffff
IEEE_DOUBLE_MASK_SIGN : Bits64
IEEE_DOUBLE_MASK_SIGN = 0x8000000000000000
IEEE_DOUBLE_INF_POS   : Bits64
IEEE_DOUBLE_INF_POS   = 0x7ff0000000000000
IEEE_DOUBLE_INF_NEG   : Bits64
IEEE_DOUBLE_INF_NEG   = 0xfff0000000000000

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

GMP_LIMB_SIZE : Integer
GMP_LIMB_SIZE = 8

GMP_LIMB_BITS : Integer
GMP_LIMB_BITS = 8 * GMP_LIMB_SIZE

GMP_LIMB_BOUND : Integer
GMP_LIMB_BOUND = (1 `prim__shl_Integer` (GMP_LIMB_BITS))

-- To estimate required count of limbs (upper bound):
-- x = the number (result)
--   length of string == number of digits <= log10(x) + 1
--   required limbs: log10(x) / log10(2^LIMB_BITS)
--     LIMB_BITS=32 -> required limbs <= number of digits / 9
--     LIMB_BITS=64 -> required limbs <= number of digits / 19
GMP_ESTIMATE_DIGITS_PER_LIMB : Integer
GMP_ESTIMATE_DIGITS_PER_LIMB = 19

twosComplement : Num a => Bits a => a -> a
twosComplement x = 1 + (complement x)

cgMkConstInteger : Int -> Integer -> Codegen (IRValue IRObjPtr)
cgMkConstInteger i val =
    do
      let absVal = abs val
      let (len ** limbs) = getLimbs absVal
      let len32 = cast {to=Bits32} $ cast {to=Int} len
      let lenForHeader = if (val >= 0) then len32 else (twosComplement len32)
      let newHeader = constHeader OBJECT_TYPE_ID_BIGINT lenForHeader
      let typeSignature = "{i64, [" ++ show len ++ " x %LimbT]}"
      cName <- addConstant i $ "private unnamed_addr addrspace(1) constant " ++ typeSignature ++ " {" ++ toIR newHeader ++ ", [" ++ show len ++ " x %LimbT] [" ++ (getLimbsIR limbs) ++ "]}, align 8"
      pure $ SSA IRObjPtr $ "bitcast (" ++ typeSignature ++ " addrspace(1)* " ++ cName ++ " to %ObjPtr)"
  where
      getLimbs : Integer -> (n:Nat ** Vect n Integer)
      getLimbs 0 = (0 ** [])
      getLimbs x = let (n ** v) = (getLimbs (x `div` GMP_LIMB_BOUND))
                       limb = (x `mod` GMP_LIMB_BOUND) in
                       ((S n) ** (limb::v))
      getLimbsIR : Vect n Integer -> String
      getLimbsIR [] = ""
      getLimbsIR (limb::[]) = "%LimbT " ++ show limb
      getLimbsIR (limb::rest) = "%LimbT " ++ show limb ++ ", " ++ (getLimbsIR rest)

cgMkIntegerSigned : IRValue I64 -> Codegen (IRValue IRObjPtr)
cgMkIntegerSigned val = do
  isNegative <- icmp "slt" val (Const I64 0)
  isZero <- icmp "eq" val (Const I64 0)
  newSize1 <- mkSelect isNegative (Const I32 (-1)) (Const I32 1)
  newSize <- mkSelect isZero (Const I32 0) newSize1
  newHeader <- mkHeader OBJECT_TYPE_ID_BIGINT newSize
  newSizeAbs <- mkAbs newSize
  allocSize <- mkMul !(mkZext newSizeAbs) (Const I64 GMP_LIMB_SIZE)
  newObj <- dynamicAllocate allocSize
  ignore $ mkIf (pure isZero) (pure (Const I1 0)) (do
       absVal <- mkAbs64 val
       putObjectSlot newObj (Const I64 0) absVal
       pure $ Const I1 0)
  putObjectHeader newObj newHeader
  pure newObj

data CompareOp = LT | LTE | EQ | GTE | GT

stringCompare : CompareOp -> Reg -> Reg -> Codegen (IRValue IRObjPtr)
stringCompare op r1 r2 = do
  o1 <- load (reg2val r1)
  o2 <- load (reg2val r2)
  h1 <- getObjectHeader o1
  h2 <- getObjectHeader o2
  l1 <- mkBinOp "and" (ConstI64 0xffffffff) h1
  l2 <- mkBinOp "and" (ConstI64 0xffffffff) h2

  minLength <- mkMin l1 l2

  lblSizeCompare <- genLabel "strcompare_size"
  lblCmpStart <- genLabel "strcompare_start"
  lblPrefixEq <- genLabel "strcompare_prefix_eq"
  lblPrefixNotEq <- genLabel "strcompare_prefix_neq"
  lblEnd <- genLabel "strcompare_end"

  jump lblSizeCompare
  beginLabel lblSizeCompare
  lengthsEqual <- icmp "eq" l1 l2

  let startCommand = case op of
                          EQ => (branch lengthsEqual lblCmpStart lblEnd)
                          _ => (branch (Const I1 1) lblCmpStart lblEnd)

  startCommand
  beginLabel lblCmpStart

  str1 <- getObjectPayloadAddr {t=I8} o1
  str2 <- getObjectPayloadAddr {t=I8} o2
  cmpResult32 <- call {t=I32} "fastcc" "@rapid.memcmp" [toIR str1, toIR str2, toIR minLength]
  cmpResult <- mkSext cmpResult32
  cmpResultIsEq <- icmp "eq" cmpResult (ConstI64 0)
  branch cmpResultIsEq lblPrefixEq lblPrefixNotEq

  beginLabel lblPrefixEq
  string1Shorter <- icmp "slt" l1 l2
  string1ShorterOrEqual <- icmp "sle" l1 l2
  string2Shorter <- icmp "slt" l2 l1
  string2ShorterOrEqual <- icmp "sle" l2 l1
  let result : IRValue I1
      result =  case op of
                     LT  => string1Shorter
                     LTE => string1ShorterOrEqual
                     EQ  => lengthsEqual
                     GTE => string2ShorterOrEqual
                     GT  => string2Shorter
  jump lblEnd

  beginLabel lblPrefixNotEq
  cmpResultIsLt <- icmp "slt" cmpResult (Const I64 0)
  cmpResultIsGt <- icmp "sgt" cmpResult (Const I64 0)
  let result2 =  case op of
                      LT  => cmpResultIsLt
                      LTE => cmpResultIsLt
                      EQ  => Const I1 0
                      GT  => cmpResultIsGt
                      GTE => cmpResultIsGt
  jump lblEnd
  beginLabel lblEnd

  finalResult <- phi [(result, lblPrefixEq), (result2, lblPrefixNotEq), (Const I1 0, lblSizeCompare)]
  cgMkInt !(mkZext finalResult)

mkSubstring : IRValue IRObjPtr -> IRValue I64 -> IRValue I64 -> Codegen (IRValue IRObjPtr)
mkSubstring strObj startIndexRaw length = do
  length32 <- mkTrunc {to=I32} !(mkMax length (Const I64 0))
  strLenBytes <- getStringByteLength strObj

  startIndex <- mkTrunc {to=I32} !(mkMax startIndexRaw (Const I64 0))
  strPayloadStart <- getObjectPayloadAddr {t=I8} strObj

  startOffset <- call {t=I32} "ccc" "@utf8_codepoints_bytelen" [toIR strPayloadStart, toIR startIndex, toIR strLenBytes]
  startAddr <- getElementPtr strPayloadStart startOffset
  maxLengthBytes <- mkMax !(mkSub strLenBytes startOffset) (Const I32 0)
  resultLength <- call {t=I32} "ccc" "@utf8_codepoints_bytelen" [toIR startAddr, toIR length32, toIR maxLengthBytes]

  newStr <- dynamicAllocate !(mkZext resultLength)
  newHeader <- mkHeader OBJECT_TYPE_ID_STR resultLength
  putObjectHeader newStr newHeader
  newStrPayload <- getObjectPayloadAddr {t=I8} newStr

  voidCall "ccc" "@llvm.memcpy.p1i8.p1i8.i64" [toIR newStrPayload, toIR startAddr, toIR !(mkZext {to=I64} resultLength), toIR (Const I1 0)]
  pure newStr

mkRuntimeCrash : Int -> String -> Codegen ()
mkRuntimeCrash i s = do
  msg <- mkStr i s
  appendCode $ "  call ccc void @idris_rts_crash_msg(" ++ toIR msg ++ ") noreturn"
  appendCode $ "unreachable"

unboxInt : IRValue (Pointer 0 IRObjPtr) -> Codegen (IRValue I64)
unboxInt src = unboxInt' !(load src)

unboxIntSigned' : Int -> IRValue IRObjPtr -> Codegen (IRValue I64)
unboxIntSigned' 8 src = mkSext =<< (mkTrunc {to=I8} =<< unboxInt' src)
unboxIntSigned' 16 src = mkSext =<< (mkTrunc {to=I16} =<< unboxInt' src)
unboxIntSigned' 32 src = mkSext =<< (mkTrunc {to=I32} =<< unboxInt' src)
unboxIntSigned' bits _ = do
  addError ("not a small int kind: " ++ show bits ++ " bits")
  mkRuntimeCrash 12345 ("not a small int kind: " ++ show bits ++ " bits")
  pure (Const I64 0)

unboxIntSigned : Int -> IRValue (Pointer 0 IRObjPtr) -> Codegen (IRValue I64)
unboxIntSigned bits reg = unboxIntSigned' bits !(load reg)

intToBits64' : IRValue IRObjPtr -> Codegen (IRValue IRObjPtr)
intToBits64' val = do
  ival <- unboxInt' val
  truncatedVal <- mkAnd (Const I64 0xffffffffffffffff) ival
  cgMkBits64 truncatedVal

unboxIntegerUnsigned : IRValue IRObjPtr -> Codegen (IRValue I64)
unboxIntegerUnsigned integerObj = do
  isZero <- icmp "eq" (Const I32 0) !(getObjectSize integerObj)
  -- get first limb (LSB)
  mkIf (pure isZero) (pure $ Const I64 0) (getObjectSlot {t=I64} integerObj 0)

unboxIntegerSigned : IRValue IRObjPtr -> Codegen (IRValue I64)
unboxIntegerSigned integerObj = do
  size <- getObjectSize integerObj
  isZero <- icmp "eq" (Const I32 0) size
  let isNegative = icmp "sgt" (Const I32 0) size
  -- get first limb (LSB)
  firstLimb <- getObjectSlot {t=I64} integerObj 0
  -- TODO: this is probably wrong for 64bit
  mkIf (pure isZero) (pure $ Const I64 0) (mkIf isNegative (mkSub (Const I64 0) firstLimb) (pure firstLimb))

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
instrAsComment i = ";" ++ (unwords $ lines $ show i)

prepareArg : Reg -> Codegen String
prepareArg Discard = do
  pure ("%ObjPtr null")
prepareArg (Loc i) = do
  tmp <- assignSSA $ "load %ObjPtr, %ObjPtr* %v" ++ (show i) ++ "Var"
  pure $ "%ObjPtr " ++ tmp
prepareArg RVal = do
  addError "cannot use rval as call arg"
  pure "error"

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

compareStr : IRValue IRObjPtr -> IRValue IRObjPtr -> Codegen (IRValue I1)
compareStr obj1 obj2 = do
  lblStart <- genLabel "strcompare_hdr"
  lblEnd <- genLabel "strcompare_end"
  lblCompareContents <- genLabel "strcompare_content"
  appendCode $ "br " ++ toIR lblStart
  beginLabel lblStart
  h1 <- getObjectHeader obj1
  h2 <- getObjectHeader obj2
  headersEqual <- icmp "eq" h1 h2
  appendCode $ "br " ++ toIR headersEqual ++ ", " ++ toIR lblCompareContents ++ ", " ++ toIR lblEnd
  beginLabel lblCompareContents
  str1 <- getObjectPayloadAddr {t=I8} obj1
  str2 <- getObjectPayloadAddr {t=I8} obj2
  length <- mkAnd h1 (ConstI64 0xffffffff)
  contentsEqual <- (SSA I1) <$> assignSSA ("call fastcc i1 @mem_eq(" ++ (showSep ", " ([toIR str1, toIR str2, toIR length])) ++ ")")
  appendCode $ "br " ++ toIR lblEnd
  beginLabel lblEnd
  phi [(headersEqual, lblStart), (contentsEqual, lblCompareContents)]
  --(SSA I1) <$> assignSSA ("phi i1 [ " ++ showWithoutType headersEqual ++ ", " ++ showWithoutType lblStart ++ " ], [ " ++ showWithoutType contentsEqual ++ ", " ++ showWithoutType lblCompareContents ++ " ]")

-- compare two BigInts `a` and `b`, return -1 if a<b, +1 if a>b, 0 otherwise
compareInteger : IRValue IRObjPtr -> IRValue IRObjPtr -> Codegen (IRValue I64)
compareInteger obj1 obj2 = do
  size1 <- getObjectSize obj1
  size2 <- getObjectSize obj2
  cmpResult <- mkIf (icmp "slt" size1 size2) (pure (Const I64 (-1))) (
    mkIf (icmp "sgt" size1 size2) (pure (Const I64 1)) (do
         limbs1 <- getObjectPayloadAddr {t=I64} obj1
         limbs2 <- getObjectPayloadAddr {t=I64} obj2
         absSize <- mkZext {to=I64} !(mkAbs size1)
         mpnResult <- call {t=I32} "ccc" "@__gmpn_cmp" [toIR limbs1, toIR limbs2, toIR absSize]
         sizeIsNegative <- icmp "slt" size1 (Const I32 0)
         mkSext !(mkSelect sizeIsNegative !(mkSub (Const I32 0) mpnResult) mpnResult)
         )
         )

  pure cmpResult

unboxChar : IRValue (Pointer 0 IRObjPtr) -> Codegen (IRValue I32)
unboxChar objPtr = do
  hdr <- getObjectHeader !(load objPtr)
  chVal64 <- mkAnd (ConstI64 0xffffffff) hdr
  chVal32 <- mkTrunc {to=I32} chVal64
  pure chVal32

assertObjectType : Reg -> Int -> Codegen ()
assertObjectType r t = assertObjectType' !(load (reg2val r)) t

mutual
getInstForConstCaseChar : {auto conNames : SortedMap Name Int} -> Int -> Reg -> List (Constant, List VMInst) -> Maybe (List VMInst) -> Codegen ()
getInstForConstCaseChar i r alts def =
  do let def' = fromMaybe [(ERROR $ "no default in const case (char)")] def
     assertObjectType r OBJECT_TYPE_ID_CHAR
     caseId <- mkVarName "case_"
     let labelEnd = caseId ++ "_end"
     scrutinee <- unboxChar (reg2val r)
     appendCode $ "  switch " ++ toIR scrutinee ++ ", label %" ++ caseId ++ "_default [ " ++ (showSep "\n      " (map (makeConstCaseLabel caseId) alts)) ++ " ]"
     appendCode $ caseId ++ "_default:"
     traverse_ (getInstIRWithComment i) def'
     appendCode $ "br label %" ++ labelEnd
     traverse_ (makeCaseAlt caseId) alts
     appendCode $ labelEnd ++ ":"
     pure ()
  where
    makeCaseAlt : String -> (Constant, List VMInst) -> Codegen ()
    makeCaseAlt caseId (Ch ch, is) = do
      let c = cast {to=Int} ch
      appendCode $ caseId ++ "_is_" ++ (show c) ++ ":"
      traverse_ (getInstIRWithComment i) is
      appendCode $ "br label %" ++ caseId ++ "_end"
    makeCaseAlt _ (c, _) = appendCode $ "ERROR: constcase must be Char, got: " ++ show c

getInstForConstCaseString : {auto conNames : SortedMap Name Int} -> Int -> Reg -> List (Constant, List VMInst) -> Maybe (List VMInst) -> Codegen ()
getInstForConstCaseString i r alts def =
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

     traverse_ (getInstIRWithComment i) def'
     appendCode $ "br " ++ toIR labelEnd

     beginLabel labelEnd
  where
    makeCaseAlt : String -> IRLabel -> IRValue IRObjPtr -> (Int, Constant, List VMInst) -> Codegen ()
    makeCaseAlt caseId labelEnd scrutinee (idx, Str s, is) = do
      let labelAltStart = MkLabel (caseId ++ "_alt_" ++ show idx)
      let labelAltNext = MkLabel (caseId ++ "_next" ++ show idx)
      compStr <- mkStr i s
      match <- compareStr compStr scrutinee
      appendCode $ "br " ++ toIR match ++ ", " ++ toIR labelAltStart ++ ", " ++ toIR labelAltNext
      -- compare s == scrut
      beginLabel labelAltStart
      traverse_ (getInstIRWithComment i) is
      appendCode $ "br " ++ toIR labelEnd
      beginLabel labelAltNext
    makeCaseAlt _ _ _ (_, c, _) = appendCode $ "ERROR: constcase must be Str, got: " ++ show c

getInstForConstCaseInteger : {auto conNames : SortedMap Name Int} -> Int -> Reg -> List (Constant, List VMInst) -> Maybe (List VMInst) -> Codegen ()
getInstForConstCaseInteger i r alts def =
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

     traverse_ (getInstIRWithComment i) def'
     appendCode $ "br " ++ toIR labelEnd

     beginLabel labelEnd
  where
    makeCaseAlt : String -> IRLabel -> IRValue IRObjPtr -> (Int, Constant, List VMInst) -> Codegen ()
    makeCaseAlt caseId labelEnd scrutinee (idx, BI bi, is) = do
      let labelAltStart = MkLabel (caseId ++ "_alt_" ++ show idx)
      let labelAltNext = MkLabel (caseId ++ "_next" ++ show idx)
      compBI <- cgMkConstInteger i bi
      match <- icmp "eq" (Const I64 0) !(compareInteger compBI scrutinee)
      appendCode $ "br " ++ toIR match ++ ", " ++ toIR labelAltStart ++ ", " ++ toIR labelAltNext
      beginLabel labelAltStart
      traverse_ (getInstIRWithComment i) is
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

genericCast : Constant -> Constant -> Reg -> Reg -> Codegen ()
genericCast fromType toType dest src =
  genericCast' fromType toType dest src (intKind fromType) (intKind toType)

  where
  genericCast' : Constant -> Constant -> Reg -> Reg -> Maybe IntKind -> Maybe IntKind -> Codegen ()
  -- to Char
  genericCast' fromType CharType dest src (Just _) _ = do
    raw <- genericIntUnbox fromType !(load (reg2val src))
    ival <- mkTrunc {to=I32} raw
    -- this also helps to rule out negative values, should fromType be signed
    surrogateUpperBound <- icmp "ult" ival (Const I32 0xe000)
    surrogateLowerBound <- icmp "ugt" ival (Const I32 0xd7ff)
    isSurrogate <- mkAnd surrogateLowerBound surrogateUpperBound
    tooHigh <- icmp "ugt" ival (Const I32 0x10ffff)
    isInvalid <- mkOr tooHigh isSurrogate
    codepoint <- mkSelect isInvalid (Const I32 0) ival
    newObj <- cgMkChar codepoint
    store newObj (reg2val dest)

  -- to Double
  genericCast' fromType DoubleType dest src (Just (Unsigned _)) _ = do
    ival <- genericIntUnbox fromType !(load (reg2val src))
    newObj <- cgMkDouble !(uitofp ival)
    store newObj (reg2val dest)
  genericCast' fromType DoubleType dest src (Just (Signed (P _))) _ = do
    ival <- genericIntUnbox fromType !(load (reg2val src))
    newObj <- cgMkDouble !(sitofp ival)
    store newObj (reg2val dest)

  -- from Double
  genericCast' DoubleType toType dest src _ (Just (Unsigned _)) = do
    f1 <- unboxDouble !(load (reg2val src))
    newObj <- genericIntBox toType !(fptoui f1)
    store newObj (reg2val dest)
  genericCast' DoubleType toType dest src _ (Just (Signed (P _))) = do
    f1 <- unboxDouble !(load (reg2val src))
    newObj <- genericIntBox toType !(fptosi f1)
    store newObj (reg2val dest)

  -- to String
  genericCast' fromType StringType dest src (Just _) _ = do
    ival <- genericIntUnbox fromType !(load (reg2val src))
    -- max size of 2^64 = 20 + (optional "-" prefix) + NUL byte (from snprintf)
    newStr <- dynamicAllocate (ConstI64 24)
    strPayload <- getObjectPayloadAddr {t=I8} newStr
    length <- (SSA I64) <$> assignSSA ("call ccc i64 @idris_rts_int_to_str(" ++ toIR strPayload ++ ", " ++ toIR ival ++ ")")
    newHeader <- mkHeader OBJECT_TYPE_ID_STR !(mkTrunc length)
    putObjectHeader newStr newHeader
    store newStr (reg2val dest)

  -- from String
  genericCast' StringType toType dest src _ (Just _) = do
    strObj <- load (reg2val src)
    parsedVal <- SSA I64 <$> assignSSA ("  call ccc i64 @idris_rts_str_to_int(" ++ toIR strObj ++ ")")
    newObj <- genericIntBox toType parsedVal
    store newObj (reg2val dest)

  -- from generic int to generic int
  genericCast' fromType IntType dest src (Just _) _ = do
    ival <- genericIntUnbox fromType !(load (reg2val src))
    newObj <- cgMkInt ival
    store newObj (reg2val dest)
  genericCast' fromType toType dest src (Just _) (Just (Unsigned bits)) = do
    ival <- genericIntUnbox fromType !(load (reg2val src))
    newObj <- if bits == 64
                 then cgMkBits64 ival
                 else do let mask = intMask bits
                         truncatedVal <- mkAnd (Const I64 mask) ival
                         cgMkInt truncatedVal
    store newObj (reg2val dest)
  genericCast' fromType toType dest src (Just _) (Just (Signed (P bits))) = do
    ival <- genericIntUnbox fromType !(load (reg2val src))
    newObj <- if bits == 64
                 then cgMkBits64 ival
                 else do let mask = intMask bits
                         truncatedVal <- mkAnd (Const I64 mask) ival
                         cgMkInt truncatedVal
    store newObj (reg2val dest)

  genericCast' fromType toType dest src _ _ = do
    addError ("cast not implemented: " ++ (show fromType) ++ " -> " ++ (show toType))

getInstForConstCaseIntLike : {auto conNames : SortedMap Name Int} -> Constant -> Int -> Reg -> List (Constant, List VMInst) -> Maybe (List VMInst) -> Codegen ()
getInstForConstCaseIntLike ty i r alts def =
  do caseId <- mkVarName "case_"
     let def' = fromMaybe [(ERROR $ "no default in const case (int)" ++ caseId)] def
     let labelEnd = caseId ++ "_end"
     scrutinee <- genericIntUnbox ty !(load $ reg2val r)
     appendCode $ "  switch " ++ toIR scrutinee ++ ", label %" ++ caseId ++ "_default [ " ++ (showSep "\n      " (map (makeConstCaseLabel caseId) alts)) ++ " ]"
     appendCode $ caseId ++ "_default:"
     traverse_ (getInstIRWithComment i) def'
     appendCode $ "br label %" ++ labelEnd
     traverse_ (makeCaseAlt caseId) alts
     appendCode $ labelEnd ++ ":"
     pure ()
  where
    makeCaseAlt : String -> (Constant, List VMInst) -> Codegen ()
    makeCaseAlt caseId (c, is) = do
      appendCode $ makeConstCaseLabelName caseId c ++ ":"
      traverse_ (getInstIRWithComment i) is
      appendCode $ "br label %" ++ caseId ++ "_end"

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

normaliseIntegerSize : IRValue IRObjPtr -> IRValue I32 -> IRValue I1 -> Codegen ()
normaliseIntegerSize integerObj maxSizeSigned invert = do
  maxSizeAbs <- mkAbs maxSizeSigned
  absRealNewSize <- mkTrunc {to=I32} !(call {t=I64} "ccc" "@rapid_bigint_real_size" [
    toIR !(getObjectPayloadAddr {t=I64} integerObj),
    toIR !(mkZext {to=I64} maxSizeAbs)
    ])
  isNegative <- icmp "slt" maxSizeSigned (Const I32 0)
  invertResult <- mkXOr isNegative invert
  signedNewSize <- mkSelect invertResult !(mkSub (Const I32 0) absRealNewSize) absRealNewSize
  newHeader <- mkHeader OBJECT_TYPE_ID_BIGINT signedNewSize
  putObjectHeader integerObj newHeader

addInteger : IRValue IRObjPtr -> IRValue IRObjPtr -> Codegen (IRValue IRObjPtr)
addInteger i1 i2 = do
      s1 <- getObjectSize i1
      s2 <- getObjectSize i2
      i1Negative <- icmp "slt" s1 (Const I32 0)
      s1a <- mkAbs s1
      s2a <- mkAbs s2
      i1longer <- icmp "ugt" s1a s2a
      -- "big" and "small" refer just to the respective limb counts
      -- it doesn't matter which number is actually bigger
      big <- mkSelect i1longer i1 i2
      small <- mkSelect i1longer i2 i1
      size1 <- mkZext {to=I64} !(mkSelect {t=I32} i1longer s1a s2a)
      size2 <- mkZext {to=I64} !(mkSelect {t=I32} i1longer s2a s1a)
      newLength <- mkAdd size1 (Const I64 1)
      newSize <- mkMul (Const I64 GMP_LIMB_SIZE) newLength
      newObj <- dynamicAllocate newSize
      carry <- call {t=I64} "ccc" "@__gmpn_add" [
        toIR !(getObjectPayloadAddr {t=I64} newObj),
        toIR !(getObjectPayloadAddr {t=I64} big),
        toIR size1,
        toIR !(getObjectPayloadAddr {t=I64} small),
        toIR size2
        ]
      putObjectSlot newObj size1 carry
      absRealNewSize <- mkAdd size1 carry
      signedNewSize <- mkSelect i1Negative !(mkSub (Const I64 0) absRealNewSize) absRealNewSize
      signedNewSize32 <- mkTrunc {to=I32} signedNewSize
      newHeader <- mkHeader OBJECT_TYPE_ID_BIGINT signedNewSize32
      putObjectHeader newObj newHeader
      pure newObj

subInteger : IRValue IRObjPtr -> IRValue IRObjPtr -> Codegen (IRValue IRObjPtr)
subInteger i1 i2 = do
      -- Subtract the smaller (by abs. value) from the larger (by abs. value)
      -- and use the sign of the larger (by abs. value) number as sign for the
      -- returned result.
      s1 <- getObjectSize i1
      s2 <- getObjectSize i2
      i1Negative <- icmp "slt" s1 (Const I32 0)
      s1a <- mkAbs s1
      s2a <- mkAbs s2
      i1longer <- icmp "ugt" s1a s2a
      i2longer <- icmp "ugt" s2a s1a
      i1bigger <- mkIf (pure i1longer) (pure $ Const I1 1) (mkIf (pure i2longer) (pure $ Const I1 0) (icmp "sgt" !(call "ccc" "@__gmpn_cmp" [
                                toIR !(getObjectPayloadAddr {t=I64} i1),
                                toIR !(getObjectPayloadAddr {t=I64} i2),
                                toIR !(mkZext {to=I64} s1a)
                                ]) (Const I32 0))
        )
      big <- mkSelect i1bigger i1 i2
      small <- mkSelect i1bigger i2 i1
      swapped <- mkSelect i1bigger (Const I1 0) (Const I1 1)
      bigSize <- getObjectSize big
      bigSizeAbs <- mkAbs bigSize
      smallSizeAbs <- mkAbs !(getObjectSize small)
      newSize <- mkMul (Const I64 GMP_LIMB_SIZE) !(mkZext {to=I64} bigSizeAbs)
      newObj <- dynamicAllocate newSize
      absDiff <- call {t=I64} "ccc" "@__gmpn_sub" [
        toIR !(getObjectPayloadAddr {t=I64} newObj),
        toIR !(getObjectPayloadAddr {t=I64} big),
        toIR !(mkZext {to=I64} bigSizeAbs),
        toIR !(getObjectPayloadAddr {t=I64} small),
        toIR !(mkZext {to=I64} smallSizeAbs)
        ]
      absRealNewSize <- call {t=I64} "ccc" "@rapid_bigint_real_size" [
        toIR !(getObjectPayloadAddr {t=I64} newObj),
        toIR !(mkZext {to=I64} bigSizeAbs)
        ]
      resultIsNegative <- mkXOr swapped i1Negative
      signedNewSize <- mkSelect resultIsNegative !(mkSub (Const I64 0) absRealNewSize) absRealNewSize
      signedNewSize32 <- mkTrunc {to=I32} signedNewSize
      newHeader <- mkHeader OBJECT_TYPE_ID_BIGINT signedNewSize32
      putObjectHeader newObj newHeader
      pure newObj

integer0 : Codegen (IRValue IRObjPtr)
integer0 = do
  newObj <- dynamicAllocate (Const I64 0)
  putObjectHeader newObj (constHeader OBJECT_TYPE_ID_BIGINT 0)
  pure newObj

andInteger : IRValue IRObjPtr -> IRValue IRObjPtr -> Codegen (IRValue IRObjPtr)
andInteger i1 i2 = do
  -- TODO: what to do with negative numbers?
  s1 <- getObjectSize i1
  s2 <- getObjectSize i2
  zero1 <- icmp "eq" s1 (Const I32 0)
  zero2 <- icmp "eq" s2 (Const I32 0)
  resultIsZero <- mkOr zero1 zero2

  mkIf (pure resultIsZero) (mkSelect zero1 i1 i2) (do
    s1a <- mkAbs s1
    s2a <- mkAbs s2
    i1longer <- icmp "ugt" s1a s2a
    -- "long" and "short" refer just to the respective limb counts
    -- it doesn't matter which number is actually bigger
    long <- mkSelect i1longer i1 i2
    short <- mkSelect i1longer i2 i1
    size1 <- mkZext {to=I64} !(mkSelect {t=I32} i1longer s1a s2a)
    size2 <- mkZext {to=I64} !(mkSelect {t=I32} i1longer s2a s1a)
    -- result can not be longer than shortest number
    let newLength = size2
    newSize <- mkMul (Const I64 GMP_LIMB_SIZE) newLength
    newObj <- dynamicAllocate newSize
    putObjectHeader newObj !(mkHeader OBJECT_TYPE_ID_BIGINT !(mkTrunc newLength))

    newLimbs <- getObjectPayloadAddr {t=I64} newObj
    shortLimbs <- getObjectPayloadAddr {t=I64} short
    longLimbs <- getObjectPayloadAddr {t=I64} long
    voidCall "ccc" "@__gmpn_and_n" [toIR newLimbs, toIR shortLimbs, toIR longLimbs, toIR newLength]

    normaliseIntegerSize newObj !(mkTrunc newLength) (Const I1 0)

    pure newObj
    )

orInteger : IRValue IRObjPtr -> IRValue IRObjPtr -> Codegen (IRValue IRObjPtr)
orInteger i1 i2 = do
  -- TODO: what to do with negative numbers?
  s1 <- getObjectSize i1
  s2 <- getObjectSize i2
  zero1 <- icmp "eq" s1 (Const I32 0)
  zero2 <- icmp "eq" s2 (Const I32 0)
  resultIsZero <- mkAnd zero1 zero2

  mkIf (pure resultIsZero) (pure i1) (do
    s1a <- mkAbs s1
    s2a <- mkAbs s2
    i1longer <- icmp "ugt" s1a s2a
    -- "big" and "small" refer just to the respective limb counts
    -- it doesn't matter which number is actually bigger
    big <- mkSelect i1longer i1 i2
    small <- mkSelect i1longer i2 i1
    size1 <- mkZext {to=I64} !(mkSelect {t=I32} i1longer s1a s2a)
    size2 <- mkZext {to=I64} !(mkSelect {t=I32} i1longer s2a s1a)
    mkIf (icmp "eq" (Const I64 0) size2) (pure big) (do
      let newLength = size1
      newSize <- mkMul (Const I64 GMP_LIMB_SIZE) newLength
      newObj <- dynamicAllocate newSize
      putObjectHeader newObj !(mkHeader OBJECT_TYPE_ID_BIGINT !(mkTrunc newLength))

      newPayload <- getObjectPayloadAddr {t=I8} newObj
      bigPayload <- getObjectPayloadAddr {t=I8} big
      appendCode $ "  call void @llvm.memcpy.p1i8.p1i8.i64(" ++ toIR newPayload ++ ", " ++ toIR bigPayload ++ ", " ++ toIR newSize++ ", i1 false)"

      newLimbs <- getObjectPayloadAddr {t=I64} newObj
      smallLimbs <- getObjectPayloadAddr {t=I64} small
      voidCall "ccc" "@__gmpn_ior_n" [toIR newLimbs, toIR newLimbs, toIR smallLimbs, toIR size2]
      pure newObj
      )
    )

xorInteger : IRValue IRObjPtr -> IRValue IRObjPtr -> Codegen (IRValue IRObjPtr)
xorInteger i1 i2 = do
  -- TODO: what to do with negative numbers?
  s1 <- getObjectSize i1
  s2 <- getObjectSize i2
  zero1 <- icmp "eq" s1 (Const I32 0)
  zero2 <- icmp "eq" s2 (Const I32 0)
  resultIsUnchanged <- mkOr zero1 zero2

  mkIf (pure resultIsUnchanged) (mkSelect zero1 i2 i1) (do
    s1a <- mkAbs s1
    s2a <- mkAbs s2
    i1longer <- icmp "ugt" s1a s2a
    -- "long" and "short" refer just to the respective limb counts
    -- it doesn't matter which number is actually bigger
    long <- mkSelect i1longer i1 i2
    short <- mkSelect i1longer i2 i1
    size1 <- mkZext {to=I64} !(mkSelect {t=I32} i1longer s1a s2a)
    size2 <- mkZext {to=I64} !(mkSelect {t=I32} i1longer s2a s1a)
    -- result can not be longer than longest number
    let newLength = size1
    newSize <- mkMul (Const I64 GMP_LIMB_SIZE) newLength
    newObj <- dynamicAllocate newSize
    putObjectHeader newObj !(mkHeader OBJECT_TYPE_ID_BIGINT !(mkTrunc newLength))

    newPayload <- getObjectPayloadAddr {t=I8} newObj
    longPayload <- getObjectPayloadAddr {t=I8} long
    appendCode $ "  call void @llvm.memcpy.p1i8.p1i8.i64(" ++ toIR newPayload ++ ", " ++ toIR longPayload ++ ", " ++ toIR newSize ++ ", i1 false)"

    newLimbs <- getObjectPayloadAddr {t=I64} newObj
    shortLimbs <- getObjectPayloadAddr {t=I64} short
    voidCall "ccc" "@__gmpn_xor_n" [toIR newLimbs, toIR newLimbs, toIR shortLimbs, toIR size2]

    normaliseIntegerSize newObj !(mkTrunc newLength) (Const I1 0)

    pure newObj
    )

mulInteger : IRValue IRObjPtr -> IRValue IRObjPtr -> Codegen (IRValue IRObjPtr)
mulInteger i1 i2 = do
      s1 <- getObjectSize i1
      s2 <- getObjectSize i2
      zero1 <- icmp "eq" s1 (Const I32 0)
      zero2 <- icmp "eq" s2 (Const I32 0)
      resultIsZero <- mkOr zero1 zero2
      mkIf (pure resultIsZero) {- then -} integer0 {- else -} (do
        sx <- mkXOr s1 s2
        signsMatch <- icmp "sge" sx (Const I32 0)
        s1a <- mkAbs s1
        s2a <- mkAbs s2
        i1longer <- icmp "ugt" s1a s2a
        -- "big" and "small" refer just to the respective limb counts
        -- it doesn't matter which number is actually bigger
        big <- mkSelect i1longer i1 i2
        small <- mkSelect i1longer i2 i1
        size1 <- mkZext {to=I64} !(mkSelect {t=I32} i1longer s1a s2a)
        size2 <- mkZext {to=I64} !(mkSelect {t=I32} i1longer s2a s1a)
        newLength <- mkAdd size1 size2
        newSize <- mkMul (Const I64 GMP_LIMB_SIZE) newLength
        newObj <- dynamicAllocate newSize
        ignore $ call {t=I64} "ccc" "@__gmpn_mul" [
          toIR !(getObjectPayloadAddr {t=I64} newObj),
          toIR !(getObjectPayloadAddr {t=I64} big),
          toIR size1,
          toIR !(getObjectPayloadAddr {t=I64} small),
          toIR size2
          ]
        absRealNewSize <- call {t=I64} "ccc" "@rapid_bigint_real_size" [
          toIR !(getObjectPayloadAddr {t=I64} newObj),
          toIR newLength
          ]
        signedNewSize <- mkSelect signsMatch absRealNewSize !(mkSub (Const I64 0) absRealNewSize)
        signedNewSize32 <- mkTrunc {to=I32} signedNewSize
        newHeader <- mkHeader OBJECT_TYPE_ID_BIGINT signedNewSize32
        putObjectHeader newObj newHeader
        pure newObj)

||| divide i1 by i2, return (quotient, remainder)
divInteger : Int -> IRValue IRObjPtr -> IRValue IRObjPtr -> Codegen (IRValue IRObjPtr, IRValue IRObjPtr)
divInteger constI i1 i2 = do
  s1 <- getObjectSize i1
  s2 <- getObjectSize i2
  s1a <- mkZext !(mkAbs s1)
  s2a <- mkZext !(mkAbs s2)
  zero1 <- icmp "eq" s1 (Const I32 0)
  zero2 <- icmp "eq" s2 (Const I32 0)
  ignore $ mkIf (pure zero2) (do
                mkRuntimeCrash constI "division by 0"
                pure (Const I1 0)
                ) (pure (Const I1 0))

  retZeroLbl <- genLabel "ret0"
  checkDividendLbl <- genLabel "div_chk"
  dividendLargerLbl <- genLabel "div_lg"
  divLbl <- genLabel "div"
  endLbl <- genLabel "div_end"

  branch zero1 retZeroLbl checkDividendLbl

  beginLabel retZeroLbl
  zeroInteger <- integer0
  jump endLbl

  beginLabel checkDividendLbl
  dividendLarger <- icmp "ugt" s2a s1a
  branch dividendLarger dividendLargerLbl divLbl

  beginLabel dividendLargerLbl
  zeroQuotient <- integer0
  jump endLbl

  beginLabel divLbl
  -- i1, i2 /= 0
  sx <- mkXOr s1 s2
  signsMatch <- icmp "sge" sx (Const I32 0)

  -- remainder can not be bigger than divisor
  let maxLimbsRemainder = s2a
  remainder <- dynamicAllocate !(mkMul (Const I64 GMP_LIMB_SIZE) maxLimbsRemainder)
  -- object must have a valid header, because the next allocation might trigger a GC
  tempHeader <- mkHeader OBJECT_TYPE_ID_BIGINT !(mkTrunc maxLimbsRemainder)
  putObjectHeader remainder tempHeader

  maxLimbsQuotient <- mkMax (Const I64 1) !(mkAdd (Const I64 1) !(mkSub s1a s2a))
  quotient <- dynamicAllocate !(mkMul (Const I64 GMP_LIMB_SIZE) maxLimbsQuotient)

  voidCall "ccc" "@__gmpn_tdiv_qr" [
    toIR !(getObjectPayloadAddr {t=I64} quotient),
    toIR !(getObjectPayloadAddr {t=I64} remainder),
    toIR (Const I64 0),
    toIR !(getObjectPayloadAddr {t=I64} i1),
    toIR s1a,
    toIR !(getObjectPayloadAddr {t=I64} i2),
    toIR s2a
    ]
  qRealNewSize <- call {t=I64} "ccc" "@rapid_bigint_real_size" [
    toIR !(getObjectPayloadAddr {t=I64} quotient),
    toIR maxLimbsQuotient
    ]
  signedNewSize <- mkSelect signsMatch qRealNewSize !(mkSub (Const I64 0) qRealNewSize)
  signedNewSize32 <- mkTrunc {to=I32} signedNewSize
  newHeader <- mkHeader OBJECT_TYPE_ID_BIGINT signedNewSize32
  putObjectHeader quotient newHeader

  i1negative <- icmp "slt" s1 (Const I32 0)
  rRealNewSize <- call {t=I64} "ccc" "@rapid_bigint_real_size" [
    toIR !(getObjectPayloadAddr {t=I64} remainder),
    toIR maxLimbsRemainder
    ]
  signedNewSize <- mkSelect i1negative !(mkSub (Const I64 0) rRealNewSize) rRealNewSize
  signedNewSize32 <- mkTrunc {to=I32} signedNewSize
  newHeader <- mkHeader OBJECT_TYPE_ID_BIGINT signedNewSize32
  putObjectHeader remainder newHeader

  jump endLbl

  beginLabel endLbl
  quotient  <- phi [(zeroInteger, retZeroLbl), (zeroQuotient, dividendLargerLbl), (quotient,  divLbl)]
  remainder <- phi [(zeroInteger, retZeroLbl), (i1,           dividendLargerLbl), (remainder, divLbl)]
  pure (quotient, remainder)

getInstIR : {auto conNames : SortedMap Name Int} -> Int -> VMInst -> Codegen ()
getInstIR i (DECLARE (Loc r)) = do
  appendCode $ "  %v" ++ show r ++ "Var = alloca %ObjPtr"
  appendCode $ "  store %ObjPtr null, %ObjPtr* %v" ++ show r ++ "Var"
getInstIR i (ASSIGN r src) = store !(load (reg2val src)) (reg2val r)

getInstIR i (OP r Crash [r1, r2]) = do
  msg <- load (reg2val r2)
  appendCode $ "  call ccc void @idris_rts_crash_msg(" ++ toIR msg ++ ") noreturn"
  appendCode $ "unreachable"
getInstIR i (ERROR s) = mkRuntimeCrash i s
getInstIR i (OP r BelieveMe [_, _, v]) = do
  store !(load (reg2val v)) (reg2val r)

getInstIR i (OP r StrHead [r1]) = do
  assertObjectType r1 OBJECT_TYPE_ID_STR
  o1 <- load (reg2val r1)
  strLength <- getStringByteLength o1
  strIsZero <- unlikely !(icmp "eq" (Const I32 0) strLength)
  strHeadOk <- genLabel "strhead_ok"
  strHeadError <- genLabel "strhead_err"
  strHeadFinished <- genLabel "strhead_finished"

  branch strIsZero strHeadError strHeadOk
  beginLabel strHeadOk
  payload <- getObjectPayloadAddr {t=I8} o1

  firstChar <- call {t=I32} "ccc" "@utf8_decode1" [toIR payload]

  newCharObj <- cgMkChar firstChar

  store newCharObj (reg2val r)
  jump strHeadFinished

  beginLabel strHeadError
  appendCode $ "call ccc void @idris_rts_crash(i64 1) noreturn"
  appendCode $ "unreachable"

  beginLabel strHeadFinished

getInstIR i (OP r StrTail [r1]) = do
  assertObjectType r1 OBJECT_TYPE_ID_STR
  o1 <- load (reg2val r1)
  strLength <- getStringLength o1
  strIsZero <- unlikely !(icmp "eq" strLength (Const I32 0))
  strTailOk <- genLabel "strtail_ok"
  strTailError <- genLabel "strtail_err"
  strTailFinished <- genLabel "strtail_finished"

  branch strIsZero strTailError strTailOk
  beginLabel strTailOk

  subStr <- mkSubstring o1 (Const I64 1) !(mkSub !(mkZext strLength) (Const I64 1))

  store subStr (reg2val r)
  jump strTailFinished

  beginLabel strTailError
  appendCode $ "call ccc void @idris_rts_crash(i64 17) noreturn"
  appendCode $ "unreachable"

  beginLabel strTailFinished

getInstIR i (OP r StrSubstr [r1, r2, r3]) = do
  assertObjectType r1 OBJECT_TYPE_ID_INT
  assertObjectType r2 OBJECT_TYPE_ID_INT
  assertObjectType r3 OBJECT_TYPE_ID_STR
  o1 <- load (reg2val r3)
  offset <- unboxInt (reg2val r1)
  length <- unboxInt (reg2val r2)
  subStr <- mkSubstring o1 offset length
  store subStr (reg2val r)

getInstIR i (OP r StrAppend [r1, r2]) = do
  assertObjectType r1 OBJECT_TYPE_ID_STR
  assertObjectType r2 OBJECT_TYPE_ID_STR
  o1 <- load (reg2val r1)
  o2 <- load (reg2val r2)
  h1 <- getObjectHeader o1
  h2 <- getObjectHeader o2
  l1 <- mkBinOp "and" (ConstI64 0xffffffff) h1
  l2 <- mkBinOp "and" (ConstI64 0xffffffff) h2
  newLength <- mkAddNoWrap l1 l2
  newStr <- dynamicAllocate newLength
  newHeader <- mkHeader OBJECT_TYPE_ID_STR !(mkTrunc newLength)

  str1 <- getObjectPayloadAddr {t=I8} o1
  str2 <- getObjectPayloadAddr {t=I8} o2

  newStrPayload1 <- getObjectPayloadAddr {t=I8} newStr
  newStrPayload2 <- getElementPtr newStrPayload1 l1

  appendCode $ "  call void @llvm.memcpy.p1i8.p1i8.i64(" ++ toIR newStrPayload1 ++ ", " ++ toIR str1 ++ ", " ++ toIR l1 ++ ", i1 false)"
  appendCode $ "  call void @llvm.memcpy.p1i8.p1i8.i64(" ++ toIR newStrPayload2 ++ ", " ++ toIR str2 ++ ", " ++ toIR l2 ++ ", i1 false)"

  putObjectHeader newStr newHeader

  store newStr (reg2val r)

getInstIR i (OP r StrReverse [r1]) = do
  assertObjectType r1 OBJECT_TYPE_ID_STR
  strObj <- load (reg2val r1)
  hdr <- getObjectHeader strObj
  length <- mkBinOp "and" (ConstI64 0xffffffff) hdr
  newStr <- dynamicAllocate length
  newHeader <- mkHeader OBJECT_TYPE_ID_STR !(mkTrunc length)

  origPayload <- getObjectPayloadAddr {t=I8} strObj
  newStrPayload <- getObjectPayloadAddr {t=I8} newStr

  appendCode $ "  call ccc void @rapid_strreverse(" ++ toIR newStrPayload ++ ", " ++ toIR origPayload ++ ", " ++ toIR length ++ ")"

  putObjectHeader newStr newHeader

  store newStr (reg2val r)

getInstIR i (OP r StrCons [r1, r2]) = do
  assertObjectType r1 OBJECT_TYPE_ID_CHAR
  assertObjectType r2 OBJECT_TYPE_ID_STR
  o1 <- load (reg2val r1)
  o2 <- load (reg2val r2)
  charVal32 <- unboxChar' o1
  l32 <- getStringByteLength o2
  -- maximum length of one codepoint in UTF-8 is 4 bytes
  newLength <- mkAddNoWrap (Const I32 4) l32
  newStr <- dynamicAllocate !(mkZext newLength)
  putObjectHeader newStr !(mkHeader OBJECT_TYPE_ID_STR newLength)

  str2 <- getObjectPayloadAddr {t=I8} o2

  newStrPayload1 <- getObjectPayloadAddr {t=I8} newStr

  charLength <- call {t=I32} "ccc" "@utf8_encode1" [toIR newStrPayload1, toIR charVal32]

  newStrPayload2 <- getElementPtr newStrPayload1 charLength
  appendCode $ "  call void @llvm.memcpy.p1i8.p1i8.i64(" ++ toIR newStrPayload2 ++ ", " ++ toIR str2 ++ ", " ++ toIR !(mkZext {to=I64} l32) ++ ", i1 false)"
  realNewLength <- mkAddNoWrap charLength l32
  putObjectHeader newStr !(mkHeader OBJECT_TYPE_ID_STR realNewLength)

  store newStr (reg2val r)

getInstIR i (OP r StrLength [r1]) = do
  assertObjectType r1 OBJECT_TYPE_ID_STR
  strObj <- load (reg2val r1)
  codepointCount <- getStringLength strObj

  sizeIntObj <- cgMkInt !(mkZext codepointCount)
  store sizeIntObj (reg2val r)
getInstIR i (OP r StrIndex [r1, r2]) = do
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

getInstIR i (OP r (LT  StringType) [r1, r2]) = store !(stringCompare LT  r1 r2) (reg2val r)
getInstIR i (OP r (LTE StringType) [r1, r2]) = store !(stringCompare LTE r1 r2) (reg2val r)
getInstIR i (OP r (EQ  StringType) [r1, r2]) = store !(stringCompare EQ  r1 r2) (reg2val r)
getInstIR i (OP r (GTE StringType) [r1, r2]) = store !(stringCompare GTE r1 r2) (reg2val r)
getInstIR i (OP r (GT  StringType) [r1, r2]) = store !(stringCompare GT  r1 r2) (reg2val r)

getInstIR i (OP r (Cast IntegerType StringType) [r1]) = do
  i1 <- load (reg2val r1)
  s1 <- getObjectSize i1
  u1 <- mkZext {to=I64} !(mkAbs s1)

  isZero <- icmp "eq" s1 (Const I32 0)

  mkIf_ (pure isZero) (do
      newStr <- mkStr i "0"
      store newStr (reg2val r)
    ) (do
      maxDigits <- call {t=TARGET_SIZE_T} "ccc" "@__gmpn_sizeinbase" [toIR !(getObjectPayloadAddr {t=MP_LIMB_T} i1), toIR u1, "i32 10"]
      isNegative <- icmp "slt" s1 (Const I32 0)

      -- we need to add one extra byte of "scratch space" for mpn_get_str
      -- if the number is negative we need one character more for the leading minus
      needsSign <- mkSelect isNegative (Const I64 2) (Const I64 1)
      maxDigitsWithSign <- mkAdd maxDigits needsSign

      newStr <- dynamicAllocate maxDigitsWithSign
      newHeader <- mkHeader OBJECT_TYPE_ID_STR !(mkTrunc maxDigitsWithSign)
      putObjectHeader newStr newHeader

      actualDigits <- call {t=I64} "ccc" "@rapid_bigint_get_str" [toIR newStr, toIR i1, "i32 10"]
      actualLengthHeader <- mkHeader OBJECT_TYPE_ID_STR !(mkTrunc actualDigits)
      putObjectHeader newStr actualLengthHeader

      store newStr (reg2val r)
    )

getInstIR i (OP r (Cast Bits64Type StringType) [r1]) = do
  obj <- load (reg2val r1)
  theBits <- unboxBits64 obj

  -- max size of 2^64 = 20 + NUL byte (from snprintf)
  newStr <- dynamicAllocate (ConstI64 24)
  strPayload <- getObjectPayloadAddr {t=I8} newStr
  length <- (SSA I64) <$> assignSSA ("call ccc i64 @idris_rts_bits64_to_str(" ++ toIR strPayload ++ ", " ++ toIR theBits ++ ")")
  newHeader <- mkHeader OBJECT_TYPE_ID_STR !(mkTrunc length)
  putObjectHeader newStr newHeader
  store newStr (reg2val r)
getInstIR i (OP r (Cast DoubleType StringType) [r1]) = do
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
getInstIR i (OP r (Cast DoubleType IntegerType) [r1]) = do
  floatObj <- load (reg2val r1)
  floatBitsAsI64 <- getObjectSlot {t=I64} floatObj 0
  exponent <- mkShiftR !(mkAnd (Const I64 $ cast IEEE_DOUBLE_MASK_EXP) floatBitsAsI64) (Const I64 52)
  -- NaN and infinity will be returned as "0"
  isInfOrNaN <- icmp "eq" exponent (Const I64 0x7ff)
  -- absolute values < 1.0 will be returned as "0"
  isSmallerThanOne <- icmp "ult" exponent (Const I64 1023)
  returnZero <- mkOr isInfOrNaN isSmallerThanOne

  newObj <- mkIf (pure returnZero) {- then -} integer0 {- else -} (do
    fraction <- mkAnd (Const I64 $ cast IEEE_DOUBLE_MASK_FRAC) floatBitsAsI64
    -- highest bit is sign bit in both, Double and I64, so we can just use that one
    isNegative <- icmp "slt" floatBitsAsI64 (Const I64 0)
    initial <- mkOr fraction (Const I64 0x10000000000000)
    toShift <- mkSub exponent (Const I64 1075)
    shiftLeft <- icmp "sgt" toShift (Const I64 0)
    mkIf (pure shiftLeft) (do
      let maxLimbCount = Const I64 17
      payloadSize <- mkMul maxLimbCount (Const I64 GMP_LIMB_SIZE)
      -- requiredBits <- (exponent - 1022)
      -- requiredLimbs <- (requiredBits+63) / 64
      -- newObj <- allocObject (requiredLimbs * 8)
      newObj <- dynamicAllocate payloadSize
      payloadAddr <- getObjectPayloadAddr {t=I8} newObj
      appendCode $ "  call void @llvm.memset.p1i8.i64(" ++ toIR payloadAddr ++ ", i8 0, " ++ toIR payloadSize ++ ", i1 false)"
      putObjectSlot newObj (Const I64 0) initial
      absRealNewSize <- call {t=I64} "ccc" "@rapid_bigint_lshift_inplace" [
        toIR !(getObjectPayloadAddr {t=I64} newObj),
        toIR maxLimbCount,
        toIR !(mkTrunc {to=I32} toShift)
      ]
      signedNewSize <- mkSelect isNegative !(mkSub (Const I64 0) absRealNewSize) absRealNewSize
      size32 <- mkTrunc {to=I32} signedNewSize
      newHeader <- mkHeader OBJECT_TYPE_ID_BIGINT size32
      putObjectHeader newObj newHeader
      pure newObj
      ) (do
        newObj <- dynamicAllocate (Const I64 8)
        toShiftRight <- mkSub (Const I64 0) toShift
        shifted <- mkShiftR initial toShiftRight
        putObjectSlot newObj (Const I64 0) shifted
        signedNewSize32 <- mkSelect isNegative (Const I32 (-1)) (Const I32 1)
        newHeader <- mkHeader OBJECT_TYPE_ID_BIGINT signedNewSize32
        putObjectHeader newObj newHeader
        pure newObj
      )
    )
  store newObj (reg2val r)

getInstIR i (OP r (Cast IntegerType DoubleType) [r1]) = do
  intObj <- load (reg2val r1)
  size <- getObjectSize intObj
  isZero <- icmp "eq" (Const I32 0) size

  mkIf_ (pure isZero) (do
      newObj <- cgMkConstDouble i 0.0
      store newObj (reg2val r)
    ) (do
    sizeAbs <- mkAbs size
    highestLimbIndex <- mkZext {to=I64} !(mkSub sizeAbs (Const I32 1))
    msbLimbAddr <- getObjectSlotAddrVar {t=I64} intObj highestLimbIndex
    msbLimb <- load msbLimbAddr
    countLeadingZeros <- SSA I64 <$> assignSSA ("  call ccc i64 @llvm.ctlz.i64(" ++ toIR msbLimb ++ ", i1 1)")
    exponentOffset <- mkAdd (Const I64 (1023 + 63)) !(mkMul highestLimbIndex (Const I64 64))
    exponent <- mkSub exponentOffset countLeadingZeros

    isInfinity <- icmp "ugt" exponent (Const I64 2046)
    doubleAsBits <- mkIf (pure isInfinity) (do
        pure (Const I64 $ cast IEEE_DOUBLE_INF_POS)
      ) (do
        fracShiftLeft <- icmp "uge" countLeadingZeros (Const I64 12)
        shiftedFraction <- mkIf (pure fracShiftLeft) (do
            mkShiftL msbLimb !(mkSub countLeadingZeros (Const I64 11))
          ) (do
            mkShiftR msbLimb !(mkSub (Const I64 11) countLeadingZeros)
          )
        fraction <- mkIf (icmp "eq" sizeAbs (Const I32 1)) (do
            pure shiftedFraction
          ) (do
            mkIf (pure fracShiftLeft) (do
                secondMsbLimbAddr <- getObjectSlotAddrVar {t=I64} intObj !(mkSub highestLimbIndex (Const I64 1))
                secondMsbLimb <- load secondMsbLimbAddr
                fractionLowerPart <- mkShiftR secondMsbLimb !(mkSub (Const I64 (64 + 11)) countLeadingZeros)
                mkOr shiftedFraction fractionLowerPart
              ) (do
                pure shiftedFraction
              )
          )
        shiftedExponent <- mkShiftL exponent (Const I64 52)
        maskedFraction <- mkAnd (Const I64 $ cast IEEE_DOUBLE_MASK_FRAC) fraction
        mkOr shiftedExponent maskedFraction
      )
    isNegative <- icmp "slt" size (Const I32 0)
    sign <- mkSelect isNegative (Const I64 $ cast IEEE_DOUBLE_MASK_SIGN) (Const I64 0)
    signedDoubleAsBits <- mkOr sign doubleAsBits
    newObj <- cgMkDoubleFromBits signedDoubleAsBits
    store newObj (reg2val r)
    )

getInstIR i (OP r (Cast StringType IntegerType) [r1]) = do
  strObj <- load (reg2val r1)
  strLength <- getObjectSize strObj
  maxLimbsCount <- mkUDiv strLength (Const I32 GMP_ESTIMATE_DIGITS_PER_LIMB)
  -- GMP requires 1 limb scratch space
  maxLimbsCountPlus1 <- mkAdd maxLimbsCount (Const I32 1)

  newObj <- dynamicAllocate !(mkZext !(mkMul maxLimbsCountPlus1 (Const I32 GMP_LIMB_SIZE)))
  putObjectHeader newObj !(mkHeader OBJECT_TYPE_ID_BIGINT maxLimbsCountPlus1)
  ignore $ call {t=I32} "ccc" "@rapid_bigint_set_str" [
    toIR newObj,
    toIR strObj
    ]
  store newObj (reg2val r)


getInstIR i (OP r (Cast StringType DoubleType) [r1]) = do
  strObj <- load (reg2val r1)
  parsedVal <- SSA F64 <$> assignSSA ("  call ccc double @idris_rts_str_to_double(" ++ toIR strObj ++ ")")
  newDouble <- cgMkDouble parsedVal
  store newDouble (reg2val r)

getInstIR i (OP r (Cast Bits8Type IntegerType) [r1]) = do
  ival <- unboxInt (reg2val r1)
  newInt <- cgMkIntegerSigned ival
  store newInt (reg2val r)
getInstIR i (OP r (Cast Bits16Type IntegerType) [r1]) = do
  ival <- unboxInt (reg2val r1)
  newInt <- cgMkIntegerSigned ival
  store newInt (reg2val r)
getInstIR i (OP r (Cast Bits32Type IntegerType) [r1]) = do
  ival <- unboxInt (reg2val r1)
  newInt <- cgMkIntegerSigned ival
  store newInt (reg2val r)
getInstIR i (OP r (Cast Bits64Type IntegerType) [r1]) = do
  ival <- unboxBits64 !(load (reg2val r1))
  isZero <- icmp "eq" (Const I64 0) ival
  newObj <- mkIf (pure isZero) (do
      newInteger <- dynamicAllocate (Const I64 0)
      putObjectHeader newInteger !(mkHeader OBJECT_TYPE_ID_BIGINT (Const I32 0))
      pure newInteger
    ) (do
      newInteger <- dynamicAllocate (Const I64 GMP_LIMB_SIZE)
      putObjectHeader newInteger !(mkHeader OBJECT_TYPE_ID_BIGINT (Const I32 1))
      putObjectSlot newInteger (Const I64 0) ival
      pure newInteger
    )
  store newObj (reg2val r)

getInstIR i (OP r (Cast Int8Type IntegerType) [r1]) = do
  ival <- unboxIntSigned 8 (reg2val r1)
  newInt <- cgMkIntegerSigned ival
  store newInt (reg2val r)
getInstIR i (OP r (Cast Int16Type IntegerType) [r1]) = do
  ival <- unboxIntSigned 16 (reg2val r1)
  newInt <- cgMkIntegerSigned ival
  store newInt (reg2val r)
getInstIR i (OP r (Cast Int32Type IntegerType) [r1]) = do
  ival <- unboxIntSigned 32 (reg2val r1)
  newInt <- cgMkIntegerSigned ival
  store newInt (reg2val r)
getInstIR i (OP r (Cast Int64Type IntegerType) [r1]) = do
  ival <- unboxBits64 !(load (reg2val r1))
  newInt <- cgMkIntegerSigned ival
  store newInt (reg2val r)

getInstIR i (OP r (Cast CharType StringType) [r1]) = do
  o1 <- load (reg2val r1)
  charVal <- unboxChar' o1
  -- maximum length of one codepoint in UTF-8 is 4 bytes
  newStr <- dynamicAllocate (Const I64 4)
  newStrPayload1 <- getObjectPayloadAddr {t=I8} newStr
  charLength <- call {t=I32} "ccc" "@utf8_encode1" [toIR newStrPayload1, toIR charVal]
  putObjectHeader newStr !(mkHeader OBJECT_TYPE_ID_STR charLength)
  store newStr (reg2val r)

getInstIR i (OP r (Cast CharType toType) [r1]) = do
  charVal <- unboxChar' !(load (reg2val r1))
  newInt <- genericIntBox toType !(mkZext charVal)
  store newInt (reg2val r)

getInstIR i (OP r (Cast IntType IntegerType) [r1]) = do
  ival <- unboxInt (reg2val r1)
  integerObj <- cgMkIntegerSigned ival
  store integerObj (reg2val r)

getInstIR i (OP r (Add Bits64Type) [r1, r2]) = bits64Binary mkAdd r r1 r2
getInstIR i (OP r (Sub Bits64Type) [r1, r2]) = bits64Binary mkSub r r1 r2
getInstIR i (OP r (Mul Bits64Type) [r1, r2]) = bits64Binary mkMul r r1 r2
getInstIR i (OP r (Div Bits64Type) [r1, r2]) = bits64Binary mkUDiv r r1 r2
getInstIR i (OP r (Mod Bits64Type) [r1, r2]) = bits64Binary mkURem r r1 r2
getInstIR i (OP r (BAnd Bits64Type) [r1, r2]) = bits64Binary mkAnd r r1 r2
getInstIR i (OP r (BOr Bits64Type) [r1, r2]) = bits64Binary mkOr r r1 r2
getInstIR i (OP r (BXOr Bits64Type) [r1, r2]) = bits64Binary mkXOr r r1 r2
getInstIR i (OP r (ShiftL Bits64Type) [r1, r2]) = bits64Binary mkShiftL r r1 r2
getInstIR i (OP r (ShiftR Bits64Type) [r1, r2]) = bits64Binary mkShiftR r r1 r2

getInstIR i (OP r (Add Int64Type) [r1, r2]) = bits64Binary mkAdd r r1 r2
getInstIR i (OP r (Sub Int64Type) [r1, r2]) = bits64Binary mkSub r r1 r2
getInstIR i (OP r (Mul Int64Type) [r1, r2]) = bits64Binary mkMul r r1 r2
getInstIR i (OP r (Div Int64Type) [r1, r2]) = bits64Binary mkSDiv r r1 r2
getInstIR i (OP r (Mod Int64Type) [r1, r2]) = bits64Binary mkSRem r r1 r2
getInstIR i (OP r (BAnd Int64Type) [r1, r2]) = bits64Binary mkAnd r r1 r2
getInstIR i (OP r (BOr Int64Type) [r1, r2]) = bits64Binary mkOr r r1 r2
getInstIR i (OP r (BXOr Int64Type) [r1, r2]) = bits64Binary mkXOr r r1 r2
getInstIR i (OP r (ShiftL Int64Type) [r1, r2]) = bits64Binary mkShiftL r r1 r2
getInstIR i (OP r (ShiftR Int64Type) [r1, r2]) = bits64Binary mkAShiftR r r1 r2

getInstIR i (OP r (Add IntType) [r1, r2]) = intBinary mkAdd r r1 r2
getInstIR i (OP r (Sub IntType) [r1, r2]) = intBinary mkSub r r1 r2
getInstIR i (OP r (Mul IntType) [r1, r2]) = intBinary mkMul r r1 r2
getInstIR i (OP r (Div IntType) [r1, r2]) = intBinary mkSDiv r r1 r2
getInstIR i (OP r (Mod IntType) [r1, r2]) = intBinary mkSRem r r1 r2
getInstIR i (OP r (BAnd IntType) [r1, r2]) = intBinary mkAnd r r1 r2
getInstIR i (OP r (BOr IntType) [r1, r2]) = intBinary mkOr r r1 r2
getInstIR i (OP r (BXOr IntType) [r1, r2]) = intBinary mkXOr r r1 r2
getInstIR i (OP r (ShiftL IntType) [r1, r2]) = intBinary mkShiftL r r1 r2
getInstIR i (OP r (ShiftR IntType) [r1, r2]) = intBinary mkShiftR r r1 r2

getInstIR i (OP r (Add IntegerType) [r1, r2]) = do
  i1 <- load (reg2val r1)
  i2 <- load (reg2val r2)

  s1 <- getObjectSize i1
  s2 <- getObjectSize i2
  sx <- mkXOr s1 s2
  signsMatch <- icmp "sge" sx (Const I32 0)
  obj <- mkIf (pure signsMatch) (addInteger i1 i2) (subInteger i1 i2)
  store obj (reg2val r)
getInstIR i (OP r (Sub IntegerType) [r1, r2]) = do
  i1 <- load (reg2val r1)
  i2 <- load (reg2val r2)

  s1 <- getObjectSize i1
  s2 <- getObjectSize i2
  sx <- mkXOr s1 s2
  signsMatch <- icmp "sge" sx (Const I32 0)
  obj <- mkIf (pure signsMatch) (subInteger i1 i2) (addInteger i1 i2)
  store obj (reg2val r)
getInstIR i (OP r (Mul IntegerType) [r1, r2]) = do
  i1 <- load (reg2val r1)
  i2 <- load (reg2val r2)
  obj <- mulInteger i1 i2
  store obj (reg2val r)
getInstIR i (OP r (Div IntegerType) [r1, r2]) = do
  i1 <- load (reg2val r1)
  i2 <- load (reg2val r2)
  (quotient, _) <- divInteger i i1 i2
  store quotient (reg2val r)
getInstIR i (OP r (Mod IntegerType) [r1, r2]) = do
  i1 <- load (reg2val r1)
  i2 <- load (reg2val r2)
  (_, remainder) <- divInteger i i1 i2
  store remainder (reg2val r)
getInstIR i (OP r (ShiftL IntegerType) [r1, r2]) = do
  integerObj <- load (reg2val r1)
  bitCount <- mkTrunc {to=I32} !(unboxIntegerUnsigned !(load (reg2val r2)))

  size <- getObjectSize integerObj
  unchanged <- mkOr !(icmp "eq" size (Const I32 0)) !(icmp "eq" bitCount (Const I32 0))
  mkIf_ (pure unchanged) (do
    store integerObj (reg2val r)
    ) (do
    sizeAbs <- mkAbs size
    fullLimbs <- mkUDiv bitCount (Const I32 GMP_LIMB_BITS)
    maxLimbsCount <- mkAdd !(mkAdd fullLimbs sizeAbs) (Const I32 1)

    newObj <- dynamicAllocate !(mkZext !(mkMul maxLimbsCount (Const I32 GMP_LIMB_SIZE)))
    lowerLimbsAddr <- getObjectPayloadAddr {t=I8} newObj
    appendCode $ "  call void @llvm.memset.p1i8.i64(" ++ toIR lowerLimbsAddr ++ ", i8 0, " ++ toIR !(mkMul !(mkZext fullLimbs) (Const I64 8)) ++ ", i1 false)"

    restBits <- mkURem bitCount (Const I32 GMP_LIMB_BITS)
    mkIf_ (icmp "ne" (Const I32 0) restBits) (do
      srcLimbs <- getObjectPayloadAddr {t=I64} integerObj
      higherLimbsAddr <- getObjectSlotAddrVar {t=I64} newObj !(mkZext {to=I64} fullLimbs)
      msbLimb <- call {t=I64} "ccc" "@__gmpn_lshift" [
        toIR higherLimbsAddr,
        toIR srcLimbs,
        toIR !(mkZext {to=I64} sizeAbs),
        toIR restBits
        ]
      msbLimbAddr <- getObjectSlotAddrVar {t=I64} newObj !(mkZext !(mkSub maxLimbsCount (Const I32 1)))
      store msbLimb msbLimbAddr
      ) (do
      srcLimbs <- getObjectPayloadAddr {t=I8} integerObj
      higherLimbsAddr <- getObjectSlotAddrVar {t=I8} newObj !(mkZext {to=I64} fullLimbs)
      voidCall "ccc" "@llvm.memcpy.p1i8.p1i8.i64" [
        toIR higherLimbsAddr,
        toIR srcLimbs,
        toIR !(mkMul !(mkZext size) (Const I64 GMP_LIMB_SIZE)),
        "i1 false"
        ]
      )

    isNegative <- icmp "slt" size (Const I32 0)
    normaliseIntegerSize newObj maxLimbsCount isNegative
    store newObj (reg2val r)
    )
getInstIR i (OP r (ShiftR IntegerType) [r1, r2]) = do
  integerObj <- load (reg2val r1)
  bitCount <- mkTrunc {to=I32} !(unboxIntegerUnsigned !(load (reg2val r2)))

  size <- getObjectSize integerObj
  unchanged <- mkOr !(icmp "eq" size (Const I32 0)) !(icmp "eq" bitCount (Const I32 0))
  mkIf_ (pure unchanged) (do
    store integerObj (reg2val r)
    ) (do
    sizeAbs <- mkAbs size
    fullLimbs <- mkUDiv bitCount (Const I32 GMP_LIMB_BITS)
    maxLimbsCount <- mkSub sizeAbs fullLimbs

    mkIf_ (icmp "sle" maxLimbsCount (Const I32 0)) (do
      store !(integer0) (reg2val r)
      ) (do
      newObj <- dynamicAllocate !(mkZext !(mkMul maxLimbsCount (Const I32 GMP_LIMB_SIZE)))

      restBits <- mkURem bitCount (Const I32 GMP_LIMB_BITS)
      mkIf_ (icmp "ne" (Const I32 0) restBits) (do
        srcHigherLimbs <- getObjectSlotAddrVar {t=I64} integerObj !(mkZext {to=I64} fullLimbs)
        dstLimbsAddr <- getObjectPayloadAddr {t=I64} newObj
        ignore $ call {t=I64} "ccc" "@__gmpn_rshift" [
          toIR dstLimbsAddr,
          toIR srcHigherLimbs,
          toIR !(mkZext {to=I64} maxLimbsCount),
          toIR restBits
          ]
        ) (do
        srcHigherLimbs <- getObjectSlotAddrVar {t=I8} integerObj !(mkZext {to=I64} fullLimbs)
        dstLimbsAddr <- getObjectPayloadAddr {t=I8} newObj
        voidCall "ccc" "@llvm.memcpy.p1i8.p1i8.i64" [
          toIR dstLimbsAddr,
          toIR srcHigherLimbs,
          toIR !(mkMul !(mkZext maxLimbsCount) (Const I64 GMP_LIMB_SIZE)),
          "i1 false"
          ]
        )

      isNegative <- icmp "slt" size (Const I32 0)
      normaliseIntegerSize newObj maxLimbsCount isNegative
      store newObj (reg2val r)
      )

    )

getInstIR i (OP r (BAnd IntegerType) [r1, r2]) = do
  i1 <- load (reg2val r1)
  i2 <- load (reg2val r2)
  obj <- andInteger i1 i2
  store obj (reg2val r)
getInstIR i (OP r (BOr IntegerType) [r1, r2]) = do
  i1 <- load (reg2val r1)
  i2 <- load (reg2val r2)
  obj <- orInteger i1 i2
  store obj (reg2val r)
getInstIR i (OP r (BXOr IntegerType) [r1, r2]) = do
  i1 <- load (reg2val r1)
  i2 <- load (reg2val r2)
  obj <- xorInteger i1 i2
  store obj (reg2val r)

getInstIR i (OP r (LT CharType) [r1, r2]) = do
  -- compare Chars by comparing their headers
  o1 <- load (reg2val r1)
  o2 <- load (reg2val r2)
  i1 <- getObjectHeader o1
  i2 <- getObjectHeader o2
  cmp_i1 <- icmp "ult" i1 i2
  cmp_i64 <- assignSSA $ "zext " ++ toIR cmp_i1 ++ " to i64"
  obj <- cgMkInt (SSA I64 cmp_i64)
  store obj (reg2val r)
getInstIR i (OP r (LTE CharType) [r1, r2]) = do
  -- compare Chars by comparing their headers
  o1 <- load (reg2val r1)
  o2 <- load (reg2val r2)
  i1 <- getObjectHeader o1
  i2 <- getObjectHeader o2
  cmp_i1 <- icmp "ule" i1 i2
  cmp_i64 <- assignSSA $ "zext " ++ toIR cmp_i1 ++ " to i64"
  obj <- cgMkInt (SSA I64 cmp_i64)
  store obj (reg2val r)
getInstIR i (OP r (GTE CharType) [r1, r2]) = do
  -- compare Chars by comparing their headers
  o1 <- load (reg2val r1)
  o2 <- load (reg2val r2)
  i1 <- getObjectHeader o1
  i2 <- getObjectHeader o2
  cmp_i1 <- icmp "uge" i1 i2
  cmp_i64 <- assignSSA $ "zext " ++ toIR cmp_i1 ++ " to i64"
  obj <- cgMkInt (SSA I64 cmp_i64)
  store obj (reg2val r)
getInstIR i (OP r (GT CharType) [r1, r2]) = do
  -- compare Chars by comparing their headers
  o1 <- load (reg2val r1)
  o2 <- load (reg2val r2)
  i1 <- getObjectHeader o1
  i2 <- getObjectHeader o2
  cmp_i1 <- icmp "ugt" i1 i2
  cmp_i64 <- assignSSA $ "zext " ++ toIR cmp_i1 ++ " to i64"
  obj <- cgMkInt (SSA I64 cmp_i64)
  store obj (reg2val r)
getInstIR i (OP r (EQ CharType) [r1, r2]) = do
  -- Two Chars are equal, iff their headers are equal
  o1 <- load (reg2val r1)
  o2 <- load (reg2val r2)
  i1 <- getObjectHeader o1
  i2 <- getObjectHeader o2
  cmp_i1 <- icmp "eq" i1 i2
  cmp_i64 <- assignSSA $ "zext " ++ toIR cmp_i1 ++ " to i64"
  obj <- cgMkInt (SSA I64 cmp_i64)
  store obj (reg2val r)

getInstIR i (OP r (LT Bits8Type) [r1, r2]) = do
  i1 <- unboxInt (reg2val r1)
  i2 <- unboxInt (reg2val r2)
  vsum_i1 <- icmp "ult" i1 i2
  vsum_i64 <- mkZext {to=I64} vsum_i1
  obj <- cgMkInt vsum_i64
  store obj (reg2val r)

getInstIR i (OP r (LT IntType) [r1, r2]) = do
  i1 <- unboxInt (reg2val r1)
  i2 <- unboxInt (reg2val r2)
  vsum_i1 <- icmp "slt" i1 i2
  vsum_i64 <- mkZext {to=I64} vsum_i1
  obj <- cgMkInt vsum_i64
  store obj (reg2val r)
getInstIR i (OP r (LTE IntType) [r1, r2]) = do
  i1 <- unboxInt (reg2val r1)
  i2 <- unboxInt (reg2val r2)
  vsum_i1 <- icmp "sle" i1 i2
  vsum_i64 <- mkZext {to=I64} vsum_i1
  obj <- cgMkInt vsum_i64
  store obj (reg2val r)
getInstIR i (OP r (EQ IntType) [r1, r2]) = do
  i1 <- unboxInt (reg2val r1)
  i2 <- unboxInt (reg2val r2)
  vsum_i1 <- icmp "eq" i1 i2
  vsum_i64 <- mkZext {to=I64} vsum_i1
  obj <- cgMkInt vsum_i64
  store obj (reg2val r)
getInstIR i (OP r (GTE IntType) [r1, r2]) = do
  i1 <- unboxInt (reg2val r1)
  i2 <- unboxInt (reg2val r2)
  vsum_i1 <- icmp "sge" i1 i2
  vsum_i64 <- mkZext {to=I64} vsum_i1
  obj <- cgMkInt vsum_i64
  store obj (reg2val r)
getInstIR i (OP r (GT IntType) [r1, r2]) = do
  i1 <- unboxInt (reg2val r1)
  i2 <- unboxInt (reg2val r2)
  vsum_i1 <- icmp "sgt" i1 i2
  vsum_i64 <- mkZext {to=I64} vsum_i1
  obj <- cgMkInt vsum_i64
  store obj (reg2val r)

getInstIR i (OP r (EQ IntegerType) [r1, r2]) = do
  intObj1 <- load (reg2val r1)
  intObj2 <- load (reg2val r2)

  cmpRaw <- compareInteger intObj1 intObj2
  cmpResult <- icmp "eq" cmpRaw (Const I64 0)

  obj <- cgMkInt !(mkZext {to=I64} cmpResult)

  store obj (reg2val r)
getInstIR i (OP r (GT IntegerType) [r1, r2]) = do
  intObj1 <- load (reg2val r1)
  intObj2 <- load (reg2val r2)

  cmpRaw <- compareInteger intObj1 intObj2
  cmpResult <- icmp "sgt" cmpRaw (Const I64 0)

  obj <- cgMkInt !(mkZext {to=I64} cmpResult)

  store obj (reg2val r)
getInstIR i (OP r (GTE IntegerType) [r1, r2]) = do
  intObj1 <- load (reg2val r1)
  intObj2 <- load (reg2val r2)

  cmpRaw <- compareInteger intObj1 intObj2
  cmpResult <- icmp "sge" cmpRaw (Const I64 0)

  obj <- cgMkInt !(mkZext {to=I64} cmpResult)

  store obj (reg2val r)
getInstIR i (OP r (LT IntegerType) [r1, r2]) = do
  intObj1 <- load (reg2val r1)
  intObj2 <- load (reg2val r2)

  cmpRaw <- compareInteger intObj1 intObj2
  cmpResult <- icmp "slt" cmpRaw (Const I64 0)

  obj <- cgMkInt !(mkZext {to=I64} cmpResult)

  store obj (reg2val r)
getInstIR i (OP r (LTE IntegerType) [r1, r2]) = do
  intObj1 <- load (reg2val r1)
  intObj2 <- load (reg2val r2)

  cmpRaw <- compareInteger intObj1 intObj2
  cmpResult <- icmp "sle" cmpRaw (Const I64 0)

  obj <- cgMkInt !(mkZext {to=I64} cmpResult)

  store obj (reg2val r)

getInstIR i (OP r (Cast fromType toType) [r1]) = genericCast fromType toType r r1

getInstIR i (OP r (LT  DoubleType) [r1, r2]) = doubleCmp "olt" r r1 r2
getInstIR i (OP r (LTE DoubleType) [r1, r2]) = doubleCmp "ole" r r1 r2
getInstIR i (OP r (EQ  DoubleType) [r1, r2]) = doubleCmp "oeq" r r1 r2
getInstIR i (OP r (GTE DoubleType) [r1, r2]) = doubleCmp "oge" r r1 r2
getInstIR i (OP r (GT  DoubleType) [r1, r2]) = doubleCmp "ogt" r r1 r2

getInstIR i (OP r (Add DoubleType) [r1, r2]) = doubleBinOp "fadd" r r1 r2
getInstIR i (OP r (Sub DoubleType) [r1, r2]) = doubleBinOp "fsub" r r1 r2
getInstIR i (OP r (Mul DoubleType) [r1, r2]) = doubleBinOp "fmul" r r1 r2
getInstIR i (OP r (Div DoubleType) [r1, r2]) = doubleBinOp "fdiv" r r1 r2
getInstIR i (OP r (Mod DoubleType) [r1, r2]) = doubleBinOp "frem" r r1 r2
getInstIR i (OP r (Neg DoubleType) [r1]) = do
  fv <- unboxFloat64 (reg2val r1)
  neg <- (SSA F64) <$> assignSSA ("fneg " ++ toIR fv)
  obj <- cgMkDouble neg
  store obj (reg2val r)

getInstIR i (OP r DoubleExp [r1]) = doubleUnaryFn "llvm.exp.f64" r r1
getInstIR i (OP r DoubleLog [r1]) = doubleUnaryFn "llvm.log.f64" r r1
getInstIR i (OP r DoubleSin [r1]) = doubleUnaryFn "llvm.sin.f64" r r1
getInstIR i (OP r DoubleCos [r1]) = doubleUnaryFn "llvm.cos.f64" r r1
getInstIR i (OP r DoubleTan [r1]) = doubleUnaryFn "tan" r r1
getInstIR i (OP r DoubleASin [r1]) = doubleUnaryFn "asin" r r1
getInstIR i (OP r DoubleACos [r1]) = doubleUnaryFn "acos" r r1
getInstIR i (OP r DoubleATan [r1]) = doubleUnaryFn "atan" r r1
getInstIR i (OP r DoubleSqrt [r1]) = doubleUnaryFn "llvm.sqrt.f64" r r1
getInstIR i (OP r DoubleFloor [r1]) = doubleUnaryFn "llvm.floor.f64" r r1
getInstIR i (OP r DoubleCeiling [r1]) = doubleUnaryFn "llvm.ceil.f64" r r1

getInstIR i (OP r (Add ty) [r1, r2]) = boundedIntBinary' (intKind ty) mkAddNoWrap mkAddNoWrap r r1 r2
getInstIR i (OP r (Sub ty) [r1, r2]) = boundedIntBinary' (intKind ty) mkSub mkSub r r1 r2
getInstIR i (OP r (Mul ty) [r1, r2]) = boundedIntBinary' (intKind ty) mkMul mkMul r r1 r2
getInstIR i (OP r (Div ty) [r1, r2]) = boundedIntBinary' (intKind ty) mkUDiv mkSDiv r r1 r2
getInstIR i (OP r (Mod ty) [r1, r2]) = boundedIntBinary' (intKind ty) mkURem mkSRem r r1 r2
getInstIR i (OP r (BAnd ty) [r1, r2]) = boundedIntBinary' (intKind ty) mkAnd mkAnd r r1 r2
getInstIR i (OP r (BOr ty) [r1, r2]) = boundedIntBinary' (intKind ty) mkOr mkOr r r1 r2
getInstIR i (OP r (BXOr ty) [r1, r2]) = boundedIntBinary' (intKind ty) mkXOr mkXOr r r1 r2
getInstIR i (OP r (ShiftL ty) [r1, r2]) = boundedIntBinary' (intKind ty) mkShiftL mkShiftL r r1 r2
getInstIR i (OP r (ShiftR ty) [r1, r2]) = boundedIntBinary' (intKind ty) mkShiftR mkAShiftR r r1 r2

getInstIR i (OP r (LT  ty) [r1, r2]) = intCompare' (intKind ty) "ult" "slt" r r1 r2
getInstIR i (OP r (LTE ty) [r1, r2]) = intCompare' (intKind ty) "ule" "sle" r r1 r2
getInstIR i (OP r (EQ  ty) [r1, r2]) = intCompare' (intKind ty) "eq"  "eq"  r r1 r2
getInstIR i (OP r (GTE ty) [r1, r2]) = intCompare' (intKind ty) "uge" "sge" r r1 r2
getInstIR i (OP r (GT  ty) [r1, r2]) = intCompare' (intKind ty) "ugt" "sgt" r r1 r2

getInstIR i (MKCON r (Left tag) args) = do
  obj <- mkCon tag !(traverse (load . reg2val) args)
  store obj (reg2val r)
getInstIR {conNames} i (MKCON r (Right n) args) = do
  case lookup n conNames of
       Just nameId => do
         loadedArgs <- traverse (load . reg2val) args
         obj <- mkCon (makeNameId nameId) loadedArgs
         store obj (reg2val r)
       Nothing => addError $ "MKCON name not found: " ++ show n

getInstIR i (MKCLOSURE r n missingN args) = do
  let missing = cast {to=Int} missingN
  let len = cast {to=Int} $ length args
  let totalArgsExpected = missing + len
  if totalArgsExpected > (cast CLOSURE_MAX_ARGS) then addError $ "ERROR : too many closure arguments: " ++ show totalArgsExpected ++ " > " ++ show CLOSURE_MAX_ARGS else do
  let header = constHeader OBJECT_TYPE_ID_CLOSURE (cast ((missing * 0x10000) + len))
  newObj <- dynamicAllocate (Const I64 $ cast (8 + 8 * len))
  putObjectHeader newObj header
  funcPtr <- (if (totalArgsExpected <= (cast FAT_CLOSURE_LIMIT))
             then
               assignSSA $ "bitcast %FuncPtrArgs" ++ show totalArgsExpected ++ " @" ++ (safeName n) ++ " to %FuncPtr"
             else do
               assignSSA $ "bitcast %FuncPtrClosureEntry @" ++ (safeName n) ++ "$$closureEntry to %FuncPtr"
               )

  putObjectSlot newObj (Const I64 0) (SSA FuncPtr funcPtr)
  for_ (enumerate args) (\iv => do
      let (i, arg) = iv
      argObj <- load {t=IRObjPtr} (reg2val arg)
      putObjectSlot newObj (Const I64 $ cast $ i+1) argObj
      pure ()
                              )
  store newObj (reg2val r)

getInstIR i (APPLY r fun arg) = do
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

getInstIR i (MKCONSTANT r (Ch c)) = do
  newObj <- cgMkChar (Const I32 $ cast c)
  store newObj (reg2val r)
getInstIR i (MKCONSTANT r (B8 c)) = do
  obj <- cgMkInt (ConstI64 $ cast c)
  store obj (reg2val r)
getInstIR i (MKCONSTANT r (B16 c)) = do
  obj <- cgMkInt (ConstI64 $ cast c)
  store obj (reg2val r)
getInstIR i (MKCONSTANT r (B32 c)) = do
  obj <- cgMkInt (ConstI64 $ cast c)
  store obj (reg2val r)
getInstIR i (MKCONSTANT r (B64 c)) = do
  obj <- cgMkBits64 (ConstI64 $ cast c)
  store obj (reg2val r)
getInstIR i (MKCONSTANT r (I8 c)) = do
  obj <- cgMkInt (ConstI64 $ cast c)
  store obj (reg2val r)
getInstIR i (MKCONSTANT r (I16 c)) = do
  obj <- cgMkInt (ConstI64 $ cast c)
  store obj (reg2val r)
getInstIR i (MKCONSTANT r (I32 c)) = do
  obj <- cgMkInt (ConstI64 $ cast c)
  store obj (reg2val r)
getInstIR i (MKCONSTANT r (I64 c)) = do
  obj <- cgMkBits64 (ConstI64 $ cast c)
  store obj (reg2val r)
getInstIR i (MKCONSTANT r (I c)) = do
  obj <- cgMkInt (ConstI64 $ (cast {to=Integer} c) `mod` 0x7fffffffffffffff)
  store obj (reg2val r)
getInstIR i (MKCONSTANT r (BI c)) = do
  obj <- cgMkConstInteger i c
  store obj (reg2val r)
getInstIR i (MKCONSTANT r (Db d)) = do
  obj <- cgMkConstDouble i d
  store obj (reg2val r)
getInstIR i (MKCONSTANT r WorldVal) = do
  obj <- mkCon 1337 []
  store obj (reg2val r)
getInstIR i (MKCONSTANT r (Str s)) = store !(mkStr i s) (reg2val r)

getInstIR i (CONSTCASE r alts def) = case findConstCaseType alts of
                                          Right (IntLikeCase ty) => getInstForConstCaseIntLike ty i r alts def
                                          Right BigIntCase => getInstForConstCaseInteger i r alts def
                                          Right StringCase => getInstForConstCaseString i r alts def
                                          Right CharCase => getInstForConstCaseChar i r alts def
                                          Left err => addError ("constcase error: " ++ err)

getInstIR {conNames} i (CASE r alts def) =
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
     traverse_ (getInstIRWithComment i) def'
     appendCode $ "br label %" ++ labelEnd
     traverse_ (makeCaseAlt caseId) alts
     appendCode $ labelEnd ++ ":"
     pure ()
  where
    makeCaseAlt : String -> (Either Int Name, List VMInst) -> Codegen ()
    makeCaseAlt caseId (Left c, is) = do
      appendCode $ caseId ++ "_tag_is_" ++ (show c) ++ ":"
      traverse_ (getInstIRWithComment i) is
      appendCode $ "br label %" ++ caseId ++ "_end"
    makeCaseAlt caseId (Right n, is) =
      case lookup n conNames of
           Just nameId => do
             appendCode $ "; " ++ (show n) ++ " -> " ++ (show nameId)
             appendCode (caseId ++ "_name_is_" ++ (show (makeNameId nameId)) ++ ":")
             traverse_ (getInstIRWithComment i) is
             appendCode ("br label %" ++ caseId ++ "_end")
           Nothing => addError $ "name for case not found: " ++ show n

getInstIR i (CALL r tailpos n args) =
  do argsV <- traverse prepareArg args
     hp <- ((++) "%RuntimePtr ") <$> assignSSA "load %RuntimePtr, %RuntimePtr* %HpVar"
     hpLim <- ((++) "%RuntimePtr ") <$> assignSSA "load %RuntimePtr, %RuntimePtr* %HpLimVar"
     let base = "%TSOPtr %BaseArg"

     let tailStr = if tailpos then "tail " else ""
     result <- assignSSA $ tailStr ++ "call fastcc %Return1 @" ++ (safeName n) ++ "(" ++ showSep ", " (hp::base::hpLim::argsV) ++ ")"

     when tailpos $ appendCode $ "ret %Return1 " ++ result
     when tailpos $ appendCode $ "unreachable"

     newHp <- assignSSA $ "extractvalue %Return1 " ++ result ++ ", 0"
     appendCode $ "store %RuntimePtr " ++ newHp ++ ", %RuntimePtr* %HpVar"
     newHpLim <- assignSSA $ "extractvalue %Return1 " ++ result ++ ", 1"
     appendCode $ "store %RuntimePtr " ++ newHpLim ++ ", %RuntimePtr* %HpLimVar"
     returnValue <- assignSSA $ "extractvalue %Return1 " ++ result ++ ", 2"
     appendCode $ "store %ObjPtr " ++ returnValue ++ ", %ObjPtr* " ++ toIR r ++ "Var"
     pure ()

getInstIR i (PROJECT r o pos) = do
  assertObjectType o OBJECT_TYPE_ID_CON_NO_ARGS
  obj <- load {t=IRObjPtr} (reg2val o)
  slot <- getObjectSlot {t=IRObjPtr} obj pos
  assertObjectTypeAny slot 0xf0
  store slot (reg2val r)

getInstIR i (EXTPRIM r n args) = compileExtPrim i n r args

getInstIR i START = pure ()
getInstIR i inst = do
  addError $ "NOT IMPLEMENTED: " ++ show inst
  mkRuntimeCrash i ("NOT IMPLEMENTED: " ++ show inst)

compileExtPrimFallback : Name -> Reg -> List Reg -> Codegen ()
compileExtPrimFallback n r args =
  do argsV <- traverse prepareArg args
     hp <- ((++) "%RuntimePtr ") <$> assignSSA "load %RuntimePtr, %RuntimePtr* %HpVar"
     hpLim <- ((++) "%RuntimePtr ") <$> assignSSA "load %RuntimePtr, %RuntimePtr* %HpLimVar"
     let base = "%TSOPtr %BaseArg"
     result <- assignSSA $ "call fastcc %Return1 @_extprim_" ++ (safeName n) ++ "(" ++ showSep ", " (hp::base::hpLim::argsV) ++ ")"

     newHp <- assignSSA $ "extractvalue %Return1 " ++ result ++ ", 0"
     appendCode $ "store %RuntimePtr " ++ newHp ++ ", %RuntimePtr* %HpVar"
     newHpLim <- assignSSA $ "extractvalue %Return1 " ++ result ++ ", 1"
     appendCode $ "store %RuntimePtr " ++ newHpLim ++ ", %RuntimePtr* %HpLimVar"
     returnValue <- SSA IRObjPtr <$> assignSSA ("extractvalue %Return1 " ++ result ++ ", 2")
     store returnValue (reg2val r)

compileExtPrim : Int -> Name -> Reg -> List Reg -> Codegen ()
compileExtPrim i (NS ns n) r args with (unsafeUnfoldNamespace ns)
  compileExtPrim i (NS ns (UN $ Basic "prim__newArray")) r [_, countReg, elemReg, _] | ["Prims", "IOArray", "Data"] = do
    lblStart <- genLabel "new_array_init_start"
    lblLoop <- genLabel "new_array_init_loop"
    lblEnd <- genLabel "new_array_init_end"
    count <- unboxInt (reg2val countReg)
    elem <- load (reg2val elemReg)
    size <- mkMul (Const I64 8) count
    newObj <- dynamicAllocate size
    hdr <- mkHeader OBJECT_TYPE_ID_IOARRAY !(mkTrunc count)
    putObjectHeader newObj hdr
    jump lblStart
    beginLabel lblStart

    jump lblLoop
    beginLabel lblLoop
    iPlus1name <- mkVarName "%iplus1."
    let iPlus1 = SSA I64 iPlus1name
    i <- phi [(Const I64 0, lblStart), (iPlus1, lblLoop)]

    addr <- getObjectSlotAddrVar newObj i
    store elem addr

    appendCode $ iPlus1name ++ " = add " ++ toIR i ++ ", 1"
    continue <- icmp "ult" iPlus1 count
    branch continue lblLoop lblEnd
    beginLabel lblEnd
    store newObj (reg2val r)

  compileExtPrim i (NS ns (UN $ Basic "prim__arrayGet")) r [_, arrReg, indexReg, _] | ["Prims", "IOArray", "Data"] = do
    index <- unboxInt (reg2val indexReg)
    array <- load (reg2val arrReg)

    addr <- getObjectSlotAddrVar array index
    val <- load addr

    store val (reg2val r)

  compileExtPrim i (NS ns (UN $ Basic "prim__arraySet")) r [_, arrReg, indexReg, valReg, _] | ["Prims", "IOArray", "Data"] = do
    index <- unboxInt (reg2val indexReg)
    array <- load (reg2val arrReg)
    val <- load (reg2val valReg)

    addr <- getObjectSlotAddrVar array index
    store val addr

  compileExtPrim i (NS ns (UN $ Basic "prim__codegen")) r [] | ["Info", "System"] = do
    store !(mkStr i "rapid") (reg2val r)
  compileExtPrim i (NS ns (UN $ Basic "prim__os")) r [] | ["Info", "System"] = do
    -- no cross compiling for now:
    store !(mkStr i System.Info.os) (reg2val r)
  compileExtPrim i (NS ns (UN $ Basic "void")) r _ | ["Uninhabited", "Prelude"] = do
    appendCode $ "  call ccc void @rapid_crash(i8* bitcast ([23 x i8]* @error_msg_void to i8*)) noreturn"
    appendCode $ "unreachable"
  compileExtPrim i (NS ns (UN $ Basic "prim__void")) r _ | ["Uninhabited", "Prelude"] = do
    appendCode $ "  call ccc void @rapid_crash(i8* bitcast ([23 x i8]* @error_msg_void to i8*)) noreturn"
    appendCode $ "unreachable"
  compileExtPrim i (NS ns (UN $ Basic "prim__newIORef")) r [_, val, _] | ["IORef", "Data"] = do
    ioRefObj <- dynamicAllocate (Const I64 8)
    putObjectHeader ioRefObj !(mkHeader OBJECT_TYPE_ID_IOREF (Const I32 0))
    putObjectSlot ioRefObj (Const I64 0) !(load $ reg2val val)
    store ioRefObj (reg2val r)
  compileExtPrim i (NS ns (UN $ Basic "prim__readIORef")) r [_, ioRefArg, _] | ["IORef", "Data"] = do
    ioRefObj <- load $ reg2val ioRefArg
    payload <- getObjectSlot ioRefObj 0
    store payload (reg2val r)
  compileExtPrim i (NS ns (UN $ Basic "prim__writeIORef")) r [_, ioRefArg, payloadArg, _] | ["IORef", "Data"] = do
    ioRefObj <- load $ reg2val ioRefArg
    payload <- load $ reg2val payloadArg
    putObjectSlot ioRefObj (Const I64 0) payload
    store !(mkUnit) (reg2val r)
  compileExtPrim i (NS ns n) r args | _ = compileExtPrimFallback (NS ns n) r args
compileExtPrim i n r args = compileExtPrimFallback n r args

getInstIRWithComment : {auto conNames : SortedMap Name Int} -> Int -> VMInst -> Codegen ()
getInstIRWithComment i instr = do
  --appendCode (instrAsComment instr)
  getInstIR i instr

appendMetadata : Int -> String -> Codegen String
appendMetadata o value = do
  i <- getUnique
  let varname = "!" ++ show (i * 1000000 + o)
  appendCode ("  " ++ varname ++ " = " ++ value)
  pure varname

getFunIR : SortedMap Name Int -> Int -> Name -> List Reg -> List VMInst -> Codegen ()
getFunIR conNames i n args body = do
    fargs <- traverse argIR args
    debug <- debugEnabled <$> getOpts
    let visibility = if debug then "external" else "private"
    debugInfo <- if (not debug) then pure "" else do
      funTmd <- appendMetadata i "!DISubroutineType(types: !{null, !0, !0, !0, !1})"
      funmd <- appendMetadata i $ "distinct !DISubprogram(name: \"" ++ safeName n ++ "\", type: " ++ funTmd ++ ", unit: !99)"
      pure $ "!dbg " ++ funmd
    appendCode ("\n\ndefine " ++ visibility ++ " fastcc %Return1 @" ++ safeName n ++ "(" ++ (showSep ", " $ prepareArgCallConv fargs) ++ ") gc \"statepoint-example\" " ++ debugInfo ++ " {")
    appendCode "entry:"
    funcEntry
    traverse_ appendCode (map copyArg args)
    traverse_ (getInstIRWithComment i) body
    funcReturn
    appendCode "}\n"
  where
    copyArg : Reg -> String
    copyArg (Loc i) = let r = show i in "  %v" ++ r ++ "Var = alloca %ObjPtr\n  store %ObjPtr %v" ++ r ++ ", %ObjPtr* %v" ++ r ++ "Var"
    copyArg _ = "ERROR: not an argument"

getFunIRClosureEntry : SortedMap Name Int -> Int -> Name -> (args : List Int) -> {auto ok : NonEmpty args} -> List VMInst -> Codegen ()
getFunIRClosureEntry conNames i n args body = do
    debug <- debugEnabled <$> getOpts
    let visibility = if debug then "external" else "private"
    appendCode ("\n\ndefine " ++ visibility ++ " fastcc %Return1 @" ++ safeName n ++ "$$closureEntry(" ++ (showSep ", " $ prepareArgCallConv ["%ObjPtr %clObj", "%ObjPtr %lastArg"]) ++ ") gc \"statepoint-example\" {")
    appendCode "entry:"
    funcEntry
    traverse_ copyArg (enumerate $ init args)
    appendCode $ "  %v" ++ (show $ last args) ++ "Var = alloca %ObjPtr"
    store (SSA IRObjPtr "%lastArg") (reg2val $ Loc $ last args)
    traverse_ (getInstIRWithComment i) body
    funcReturn
    appendCode "}\n"
  where
    copyArg : (Int, Int) -> Codegen ()
    copyArg (index, i) =
      let clObj = SSA IRObjPtr "%clObj" in do
        appendCode $ "  %v" ++ show i ++ "Var = alloca %ObjPtr"
        arg <- getObjectSlot clObj (index + 1)
        store arg (reg2val (Loc i))

builtinForeign : (n : Nat ** (Vect n (IRValue IRObjPtr) -> Codegen ())) -> Name -> (argTypes : List CFType) -> CFType -> Codegen ()
builtinForeign builtin name argTypes ret = do
  let (n ** f) = builtin
  appendCode ("define external fastcc %Return1 @" ++ safeName name ++ "(" ++ (showSep ", " $ prepareArgCallConv $ toList $ map toIR (args n)) ++ ") gc \"statepoint-example\" {")
  funcEntry
  f (args n)
  funcReturn
  appendCode "\n}\n"
  where
  args : (n : Nat) -> Vect n (IRValue IRObjPtr)
  args n = map (\i => SSA IRObjPtr $ "%arg" ++ show (finToNat i)) range


foreignRedirectMap : List (String, String)
foreignRedirectMap = [
    ("C:idris2_openFile, libidris2_support, idris_file.h", "rapid_system_file_open")
  , ("C:fdopen,libc 6", "rapid_system_fdopen")
  , ("C:idris2_closeFile, libidris2_support, idris_file.h", "rapid_system_file_close")
  , ("C:fflush,libc 6", "rapid_system_file_flush")
  , ("C:idris2_fileSize, libidris2_support, idris_file.h", "rapid_system_file_size")
  , ("C:idris2_fileAccessTime, libidris2_support, idris_file.h", "rapid_system_file_atime")
  , ("C:idris2_fileStatusTime, libidris2_support, idris_file.h", "rapid_system_file_ctime")
  , ("C:idris2_fileModifiedTime, libidris2_support, idris_file.h", "rapid_system_file_mtime")
  , ("C:idris2_readLine, libidris2_support, idris_file.h", "rapid_system_file_read_line")
  , ("C:idris2_readChars, libidris2_support, idris_file.h", "rapid_system_file_read_chars")
  , ("C:idris2_seekLine, libidris2_support, idris_file.h", "rapid_system_file_seek_line")
  , ("C:fgetc,libc 6", "rapid_system_file_read_char")
  , ("C:idris2_chmod, libidris2_support, idris_file.h", "rapid_system_file_chmod")
  , ("C:getchar,libc 6", "rapid_system_getchar")
  , ("C:putchar,libc 6", "rapid_system_putchar")
  , ("C:idris2_getStr, libidris2_support, idris_support.h", "rapid_system_stdin_getline")
  , ("C:idris2_writeLine, libidris2_support, idris_file.h", "rapid_system_file_write_string")
  , ("C:idris2_eof, libidris2_support, idris_file.h", "rapid_system_file_eof")
  , ("C:idris2_removeFile, libidris2_support, idris_file.h", "rapid_system_file_remove")
  , ("C:idris2_fileError, libidris2_support, idris_file.h", "rapid_system_file_error")
  , ("C:idris2_fileErrno, libidris2_support, idris_file.h", "rapid_system_errno")
  , ("C:idris2_getErrno, libidris2_support, idris_support.h", "rapid_system_errno")
  , ("C:idris2_stdin, libidris2_support, idris_file.h", "rapid_system_file_stdin")
  , ("C:idris2_stdout, libidris2_support, idris_file.h", "rapid_system_file_stdout")
  , ("C:idris2_stderr, libidris2_support, idris_file.h", "rapid_system_file_stderr")
  , ("C:idris2_currentDirectory, libidris2_support, idris_directory.h", "rapid_system_current_dir")
  , ("C:idris2_createDir, libidris2_support, idris_directory.h", "rapid_system_dir_create")
  , ("C:idris2_changeDir, libidris2_support, idris_directory.h", "rapid_system_dir_change")
  , ("C:idris2_removeDir, libidris2_support, idris_directory.h", "rapid_system_dir_remove")
  , ("C:idris2_openDir, libidris2_support, idris_directory.h", "rapid_system_dir_open")
  , ("C:idris2_closeDir, libidris2_support, idris_directory.h", "rapid_system_dir_close")
  , ("C:idris2_nextDirEntry, libidris2_support, idris_directory.h", "rapid_system_dir_next_entry")
  , ("C:idris2_popen, libidris2_support, idris_file.h", "rapid_system_popen")
  , ("C:idris2_pclose, libidris2_support, idris_file.h", "rapid_system_pclose")
  , ("C:idris2_free, libidris2_support, idris_memory.h", "rapid_system_free")
  , ("C:idris2_putStr, libidris2_support, idris_support.h", "rapid_putstr")
  , ("C:idris2_readBufferData, libidris2_support, idris_file.h", "idris_rts_read_buffer_data")
  , ("C:idris2_writeBufferData, libidris2_support, idris_file.h", "idris_rts_write_buffer_data")
  , ("C:idris2_isNull, libidris2_support, idris_support.h", "prim/isNull")
  , ("C:idris2_fileErrno, libidris2_suppor, idris_support.h", "rapid_system_file_errno")
  , ("C:idrnet_errno, libidris2_support, idris_net.h", "rapid_system_errno")
  , ("C:idris2_strerror, libidris2_support, idris_support.h", "rapid_system_strerror")
  , ("C:idris2_getString, libidris2_support, idris_support.h", "prim/getString")
  , ("C:strlen,libc 6", "rapid_string_bytelength") -- <= remove, when Idris2 PR #1261 is merged

  , ("C:idris2_setupTerm, libidris2_support, idris_term.h", "idris2_setupTerm")
  , ("C:idris2_getTermCols, libidris2_support, idris_term.h", "idris2_getTermCols")
  , ("C:idris2_getTermLines, libidris2_support, idris_term.h", "idris2_getTermLines")

  , ("scheme:blodwen-stringbytelen", "rapid_string_bytelength")
  , ("scheme:blodwen-string-iterator-new", "prim/blodwen-string-iterator-new")
  , ("scheme:blodwen-string-iterator-next", "prim/blodwen-string-iterator-next")
  , ("scheme:blodwen-string-iterator-to-string", "prim/blodwen-string-iterator-to-string")
  , ("C:exit, libc 6", "rapid_system_exit")
  , ("C:idris2_system, libidris2_support, idris_system.h", "rapid_system_system")
  , ("C:getenv, libc 6", "rapid_system_get_env")
  , ("scheme:blodwen-arg-count", "rapid_system_get_arg_count")
  , ("scheme:blodwen-arg", "rapid_system_get_arg")

  , ("C:idrnet_af_inet, libidris2_support, idris_net.h", "idrnet_af_inet")
  , ("C:idrnet_af_inet6, libidris2_support, idris_net.h", "idrnet_af_inet6")
  , ("C:idrnet_af_unix, libidris2_support, idris_net.h", "idrnet_af_unix")
  , ("C:idrnet_af_unspec, libidris2_support, idris_net.h", "idrnet_af_unspec")
  , ("C:idrnet_accept, libidris2_support, idris_net.h", "idrnet_accept")
  , ("C:idrnet_bind, libidris2_support, idris_net.h", "idrnet_bind")
  , ("C:idrnet_create_sockaddr, libidris2_support, idris_net.h", "idrnet_create_sockaddr")
  , ("C:idrnet_free, libidris2_support, idris_net.h", "idrnet_free")
  , ("C:idrnet_fdopen, libidris2_support, idris_net.h", "rapid_system_fdopen")
  , ("C:idrnet_sockaddr_family, libidris2_support, idris_net.h", "idrnet_sockaddr_family")
  , ("C:idrnet_sockaddr_ipv4, libidris2_support, idris_net.h", "idrnet_sockaddr_ipv4")
  , ("C:idrnet_sockaddr_unix, libidris2_support, idris_net.h", "idrnet_sockaddr_unix")
  , ("C:idrnet_socket, libidris2_support, idris_net.h", "idrnet_socket")
  , ("C:idrnet_listen, libidris2_support, idris_net.h", "idrnet_listen")

  , ("scheme:blodwen-buffer-size", "prim/blodwen-buffer-size")
  , ("scheme:blodwen-new-buffer", "prim/blodwen-new-buffer")
  , ("scheme:blodwen-buffer-free", "prim/noop2")
  , ("scheme:blodwen-buffer-setbyte", "prim/blodwen-buffer-setbyte")
  , ("scheme:blodwen-buffer-getbyte", "prim/blodwen-buffer-getbyte")
  , ("scheme:blodwen-buffer-setbits16", "prim/blodwen-buffer-setbits16")
  , ("scheme:blodwen-buffer-getbits16", "prim/blodwen-buffer-getbits16")
  , ("scheme:blodwen-buffer-setbits32", "prim/blodwen-buffer-setbits32")
  , ("scheme:blodwen-buffer-getbits32", "prim/blodwen-buffer-getbits32")
  , ("scheme:blodwen-buffer-setbits64", "prim/blodwen-buffer-setbits64")
  , ("scheme:blodwen-buffer-getbits64", "prim/blodwen-buffer-getbits64")
  , ("scheme:blodwen-buffer-setint32", "prim/blodwen-buffer-setint32")
  , ("scheme:blodwen-buffer-getint32", "prim/blodwen-buffer-getint32")
  , ("scheme:blodwen-buffer-setint", "prim/blodwen-buffer-setint")
  , ("scheme:blodwen-buffer-getint", "prim/blodwen-buffer-getint")
  , ("scheme:blodwen-buffer-setdouble", "prim/blodwen-buffer-setdouble")
  , ("scheme:blodwen-buffer-getdouble", "prim/blodwen-buffer-getdouble")
  , ("scheme:blodwen-buffer-setstring", "prim/blodwen-buffer-setstring")
  , ("scheme:blodwen-buffer-getstring", "prim/blodwen-buffer-getstring")
  , ("scheme:blodwen-buffer-copydata", "prim/blodwen-buffer-copydata")

  , ("scheme:blodwen-thread", "rapid_system_fork")

  , ("scheme:blodwen-clock-time-utc", "prim/blodwen-clock-time-utc")
  , ("scheme:blodwen-clock-time-monotonic", "prim/blodwen-clock-time-monotonic")
  , ("scheme:blodwen-clock-time-duration", "prim/blodwen-clock-time-duration")
  , ("scheme:blodwen-clock-time-process", "prim/blodwen-clock-time-process")
  , ("scheme:blodwen-clock-time-thread", "prim/blodwen-clock-time-thread")
  , ("scheme:blodwen-clock-time-gccpu", "prim/blodwen-clock-time-gccpu")
  , ("scheme:blodwen-clock-time-gcreal", "prim/blodwen-clock-time-gcreal")

  , ("scheme:blodwen-is-time?", "prim/blodwen-is-time")
  , ("scheme:blodwen-clock-second", "prim/blodwen-clock-second")
  , ("scheme:blodwen-clock-nanosecond", "prim/blodwen-clock-nanosecond")

  , ("scheme:string-concat", "prim/string-concat")
  , ("scheme:string-pack", "prim/string-pack")
  , ("scheme:string-unpack", "prim/string-unpack")
  ]

findForeignName : List String -> Maybe String
findForeignName cs =
  case find (isPrefixOf "rapid:") cs of
       Just found => Just (substr 6 99999 found)
       Nothing => choiceMap (\n => lookup n foreignRedirectMap) cs

getForeignFunctionIR : Int -> Name -> List String -> List CFType -> CFType -> Codegen ()
getForeignFunctionIR i name cs args ret = do
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
                              neArgs@(_::_) => runCodegen opts $ getFunIRClosureEntry conNames ((2*i + 1)+1000) n neArgs body
                              in
      (runCodegen opts $ getFunIR conNames ((2*i)+1000) n (map Loc args) body) ++ closureEntry where
getVMIR opts conNames (i, (n, MkVMForeign cs args ret)) =
  let debug = debugEnabled opts in
      (runCodegen opts $ getForeignFunctionIR i n cs args ret) ++ "\n"
getVMIR _ _ (i, (n, MkVMError is)) = ""

funcPtrTypes : String
funcPtrTypes = fastConcat $ map funcPtr (rangeFromTo 0 FAT_CLOSURE_LIMIT) where
  funcPtr : Int -> String
  funcPtr i = "%FuncPtrArgs" ++ (show (i + 1)) ++ " = type %Return1 (%RuntimePtr, %TSOPtr, %RuntimePtr" ++ repeatStr ", %ObjPtr" (integerToNat $ cast (i+1)) ++ ")*\n"

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
