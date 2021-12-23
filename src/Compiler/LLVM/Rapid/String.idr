module Compiler.LLVM.Rapid.String

import Data.Vect

import Compiler.LLVM.IR
import Compiler.LLVM.Instruction
import Compiler.LLVM.Rapid.Object
import Control.Codegen
import Data.Utils

public export
data CompareOp = LT | LTE | EQ | GTE | GT

export
getStringLength : IRValue IRObjPtr -> Codegen (IRValue I32)
getStringLength strObj = do
  strLenBytes <- getStringByteLength strObj
  call {t=I32} "ccc" "@utf8_bytes_to_codepoints" [toIR !(getObjectPayloadAddr {t=I8} strObj), toIR strLenBytes]

export
stringCompare : CompareOp -> IRValue IRObjPtr -> IRValue IRObjPtr -> Codegen (IRValue IRObjPtr)
stringCompare op o1 o2 = do
  l1 <- getObjectSize o1
  l2 <- getObjectSize o2

  minLength <- mkZext {to=I64} !(mkMin l1 l2)

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

export
stringEqual : IRValue IRObjPtr -> IRValue IRObjPtr -> Codegen (IRValue I1)
stringEqual obj1 obj2 = do
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

export
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

export
stringHead : IRValue IRObjPtr -> Codegen (IRValue IRObjPtr)
stringHead o1 = do
  strLength <- getStringByteLength o1
  strIsZero <- unlikely !(icmp "eq" (Const I32 0) strLength)

  mkIf (pure strIsZero) (do
        appendCode $ "call ccc void @idris_rts_crash(i64 1) noreturn"
        appendCode $ "unreachable"
        pure nullPtr
        ) (do
        payload <- getObjectPayloadAddr {t=I8} o1
        firstChar <- call {t=I32} "ccc" "@utf8_decode1" [toIR payload]
        cgMkChar firstChar
        )

export
stringTail : IRValue IRObjPtr -> Codegen (IRValue IRObjPtr)
stringTail o1 = do
  strLength <- getStringLength o1
  strIsZero <- unlikely !(icmp "eq" strLength (Const I32 0))

  mkIf (pure strIsZero) (do
    appendCode $ "call ccc void @idris_rts_crash(i64 17) noreturn"
    appendCode $ "unreachable"
    pure nullPtr
    ) (do
    mkSubstring o1 (Const I64 1) !(mkSub !(mkZext strLength) (Const I64 1))
    )

export
stringAppend : IRValue IRObjPtr -> IRValue IRObjPtr -> Codegen (IRValue IRObjPtr)
stringAppend o1 o2 = do
  l1 <- getObjectSize o1
  l2 <- getObjectSize o2
  newLength <- mkAddNoWrap l1 l2
  newStr <- dynamicAllocate !(mkZext newLength)
  newHeader <- mkHeader OBJECT_TYPE_ID_STR newLength

  str1 <- getObjectPayloadAddr {t=I8} o1
  str2 <- getObjectPayloadAddr {t=I8} o2

  l1_64 <- mkZext {to=I64} l1
  l2_64 <- mkZext {to=I64} l2
  newStrPayload1 <- getObjectPayloadAddr {t=I8} newStr
  newStrPayload2 <- getElementPtr newStrPayload1 l1_64

  appendCode $ "  call void @llvm.memcpy.p1i8.p1i8.i64(" ++ toIR newStrPayload1 ++ ", " ++ toIR str1 ++ ", " ++ toIR l1_64 ++ ", i1 false)"
  appendCode $ "  call void @llvm.memcpy.p1i8.p1i8.i64(" ++ toIR newStrPayload2 ++ ", " ++ toIR str2 ++ ", " ++ toIR l2_64 ++ ", i1 false)"

  putObjectHeader newStr newHeader
  pure newStr

export
stringReverse : IRValue IRObjPtr -> Codegen (IRValue IRObjPtr)
stringReverse strObj = do
  length <- getObjectSize strObj
  newStr <- dynamicAllocate !(mkZext length)
  newHeader <- mkHeader OBJECT_TYPE_ID_STR length

  origPayload <- getObjectPayloadAddr {t=I8} strObj
  newStrPayload <- getObjectPayloadAddr {t=I8} newStr

  length64 <- mkZext {to=I64} length
  appendCode $ "  call ccc void @rapid_strreverse(" ++ toIR newStrPayload ++ ", " ++ toIR origPayload ++ ", " ++ toIR length64 ++ ")"

  putObjectHeader newStr newHeader
  pure newStr

export
stringCons : IRValue IRObjPtr -> IRValue IRObjPtr -> Codegen (IRValue IRObjPtr)
stringCons charObj strObj = do
  charVal32 <- unboxChar' charObj
  l32 <- getStringByteLength strObj
  -- maximum length of one codepoint in UTF-8 is 4 bytes
  newLength <- mkAddNoWrap (Const I32 4) l32
  newStr <- dynamicAllocate !(mkZext newLength)
  putObjectHeader newStr !(mkHeader OBJECT_TYPE_ID_STR newLength)

  str2 <- getObjectPayloadAddr {t=I8} strObj

  newStrPayload1 <- getObjectPayloadAddr {t=I8} newStr

  charLength <- call {t=I32} "ccc" "@utf8_encode1" [toIR newStrPayload1, toIR charVal32]

  newStrPayload2 <- getElementPtr newStrPayload1 charLength
  appendCode $ "  call void @llvm.memcpy.p1i8.p1i8.i64(" ++ toIR newStrPayload2 ++ ", " ++ toIR str2 ++ ", " ++ toIR !(mkZext {to=I64} l32) ++ ", i1 false)"
  realNewLength <- mkAddNoWrap charLength l32
  putObjectHeader newStr !(mkHeader OBJECT_TYPE_ID_STR realNewLength)
  pure newStr
