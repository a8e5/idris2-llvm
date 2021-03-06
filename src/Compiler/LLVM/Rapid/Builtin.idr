module Compiler.LLVM.Rapid.Builtin

import Data.Vect
import System.Info

import Core.Name

import Compiler.LLVM.IR
import Compiler.LLVM.Instruction
import Compiler.LLVM.Rapid.Foreign
import Compiler.LLVM.Rapid.Object
import Control.Codegen
import Data.Utils
import Rapid.Common

-- we provide our own in Data.Utils
%hide Core.Name.Namespace.showSep

mk_prim__bufferNew : Vect 2 (IRValue IRObjPtr) -> Codegen (IRValue IRObjPtr)
mk_prim__bufferNew [sizeObj, _] = do
  size <- unboxInt' sizeObj
  -- TODO: safety check: size < 2^32
  hdrValue <- mkHeader OBJECT_TYPE_ID_BUFFER !(mkTrunc size)
  newObj <- dynamicAllocate size
  putObjectHeader newObj hdrValue
  pure newObj

mk_prim__bufferSize : Vect 1 (IRValue IRObjPtr) -> Codegen (IRValue IRObjPtr)
mk_prim__bufferSize [arg0] = do
  hdr <- getObjectHeader arg0
  size <- mkAnd hdr (ConstI64 0xffffffff)
  sizeInt <- cgMkInt size
  pure sizeInt

mk_prim__bufferGetByte : Vect 3 (IRValue IRObjPtr) -> Codegen (IRValue IRObjPtr)
mk_prim__bufferGetByte [buf, offsetObj, _] = do
  -- TODO: size check in safe mode
  --hdr <- getObjectHeader buf
  --size <- mkAnd hdr (ConstI64 0xffffffff)
  offset <- unboxInt' offsetObj
  payloadStart <- getObjectPayloadAddr {t=I8} buf
  bytePtr <- getElementPtr payloadStart offset
  byte <- load bytePtr
  val <- mkZext {to=I64} byte
  cgMkInt val

mk_prim__bufferSetByte : Vect 4 (IRValue IRObjPtr) -> Codegen (IRValue IRObjPtr)
mk_prim__bufferSetByte [buf, offsetObj, valObj, _] = do
  -- TODO: size check in safe mode
  --hdr <- getObjectHeader buf
  --size <- mkAnd hdr (ConstI64 0xffffffff)
  offset <- unboxInt' offsetObj
  payloadStart <- getObjectPayloadAddr {t=I8} buf
  bytePtr <- getElementPtr payloadStart offset
  val <- mkTrunc {to=I8} !(unboxInt' valObj)
  store val bytePtr
  mkUnit

mk_prim__bufferGetDouble : Vect 3 (IRValue IRObjPtr) -> Codegen (IRValue IRObjPtr)
mk_prim__bufferGetDouble [buf, offsetObj, _] = do
  -- TODO: size check in safe mode
  --hdr <- getObjectHeader buf
  --size <- mkAnd hdr (ConstI64 0xffffffff)
  offset <- unboxInt' offsetObj
  payloadStart <- getObjectPayloadAddr {t=I8} buf
  bytePtr <- getElementPtr payloadStart offset
  doublePtr <- bitcastA {n=1} bytePtr
  val <- load doublePtr
  cgMkDouble val

mk_prim__bufferSetDouble : Vect 4 (IRValue IRObjPtr) -> Codegen (IRValue IRObjPtr)
mk_prim__bufferSetDouble [buf, offsetObj, valObj, _] = do
  -- TODO: size check in safe mode
  --hdr <- getObjectHeader buf
  --size <- mkAnd hdr (ConstI64 0xffffffff)
  offset <- unboxInt' offsetObj
  payloadStart <- getObjectPayloadAddr {t=I8} buf
  bytePtr <- getElementPtr payloadStart offset
  doublePtr <- bitcastA {n=1} bytePtr
  val <- unboxFloat64' valObj
  store val doublePtr
  mkUnit

mk_prim__bufferGetInt : Vect 3 (IRValue IRObjPtr) -> Codegen (IRValue IRObjPtr)
mk_prim__bufferGetInt [buf, offsetObj, _] = do
  -- TODO: size check in safe mode
  --hdr <- getObjectHeader buf
  --size <- mkAnd hdr (ConstI64 0xffffffff)
  offset <- unboxInt' offsetObj
  payloadStart <- getObjectPayloadAddr {t=I8} buf
  bytePtr <- getElementPtr payloadStart offset
  intPtr <- bitcastA {to=I64} {n=1} bytePtr
  val <- load intPtr
  cgMkInt val

mk_prim__bufferGetInt32 : Vect 3 (IRValue IRObjPtr) -> Codegen (IRValue IRObjPtr)
mk_prim__bufferGetInt32 [buf, offsetObj, _] = do
  -- TODO: size check in safe mode
  --hdr <- getObjectHeader buf
  --size <- mkAnd hdr (ConstI64 0xffffffff)
  offset <- unboxInt' offsetObj
  payloadStart <- getObjectPayloadAddr {t=I8} buf
  bytePtr <- getElementPtr payloadStart offset
  intPtr <- bitcastA {to=I32} {n=1} bytePtr
  val32 <- load intPtr
  val <- mkZext val32
  cgMkInt val

mk_prim__bufferSetInt : Vect 4 (IRValue IRObjPtr) -> Codegen (IRValue IRObjPtr)
mk_prim__bufferSetInt [buf, offsetObj, valObj, _] = do
  -- TODO: size check in safe mode
  --hdr <- getObjectHeader buf
  --size <- mkAnd hdr (ConstI64 0xffffffff)
  offset <- unboxInt' offsetObj
  payloadStart <- getObjectPayloadAddr {t=I8} buf
  bytePtr <- getElementPtr payloadStart offset
  intPtr <- bitcastA {to=I64} {n=1} bytePtr
  val <- unboxInt' valObj
  store val intPtr
  mkUnit

mk_prim__bufferSetInt32 : Vect 4 (IRValue IRObjPtr) -> Codegen (IRValue IRObjPtr)
mk_prim__bufferSetInt32 [buf, offsetObj, valObj, _] = do
  -- TODO: size check in safe mode
  --hdr <- getObjectHeader buf
  --size <- mkAnd hdr (ConstI64 0xffffffff)
  offset <- unboxInt' offsetObj
  payloadStart <- getObjectPayloadAddr {t=I8} buf
  bytePtr <- getElementPtr payloadStart offset
  intPtr <- bitcastA {to=I32} {n=1} bytePtr
  val <- mkTrunc {to=I32} !(unboxInt' valObj)
  store val intPtr
  mkUnit

mk_prim__bufferGetBits16 : Vect 3 (IRValue IRObjPtr) -> Codegen (IRValue IRObjPtr)
mk_prim__bufferGetBits16 [buf, offsetObj, _] = do
  -- TODO: this assumes little-endian target architecture
  -- TODO: size check in safe mode
  --hdr <- getObjectHeader buf
  --size <- mkAnd hdr (ConstI64 0xffffffff)
  offset <- unboxInt' offsetObj
  payloadStart <- getObjectPayloadAddr {t=I8} buf
  bytePtr <- getElementPtr payloadStart offset
  bitsPtr <- bitcastA {to=I16} {n=1} bytePtr
  valRaw <- load bitsPtr
  val <- mkZext valRaw
  cgMkInt val

mk_prim__bufferSetBits16 : Vect 4 (IRValue IRObjPtr) -> Codegen (IRValue IRObjPtr)
mk_prim__bufferSetBits16 [buf, offsetObj, valObj, _] = do
  -- TODO: this assumes little-endian target architecture
  -- TODO: size check in safe mode
  --hdr <- getObjectHeader buf
  --size <- mkAnd hdr (ConstI64 0xffffffff)
  offset <- unboxInt' offsetObj
  payloadStart <- getObjectPayloadAddr {t=I8} buf
  bytePtr <- getElementPtr payloadStart offset
  bitsPtr <- bitcastA {to=I16} {n=1} bytePtr
  val <- mkTrunc {to=I16} !(unboxInt' valObj)
  store val bitsPtr
  mkUnit

mk_prim__bufferGetString : Vect 4 (IRValue IRObjPtr) -> Codegen (IRValue IRObjPtr)
mk_prim__bufferGetString [buf, offsetObj, lengthObj, _] = do
  -- TODO: size check in safe mode
  --hdr <- getObjectHeader buf
  --size <- mkAnd hdr (ConstI64 0xffffffff)
  offset <- unboxInt' offsetObj
  length <- unboxInt' lengthObj
  payloadStart <- getObjectPayloadAddr {t=I8} buf
  bytePtr <- getElementPtr payloadStart offset

  newStr <- dynamicAllocate length
  newHeader <- mkHeader OBJECT_TYPE_ID_STR !(mkTrunc length)
  putObjectHeader newStr newHeader
  strPayload <- getObjectPayloadAddr {t=I8} newStr
  appendCode $ "  call void @llvm.memcpy.p1i8.p1i8.i64(" ++ toIR strPayload ++ ", " ++ toIR bytePtr ++ ", " ++ toIR length ++ ", i1 false)"
  pure newStr

mk_prim__bufferSetString : Vect 4 (IRValue IRObjPtr) -> Codegen (IRValue IRObjPtr)
mk_prim__bufferSetString [buf, offsetObj, valObj, _] = do
  -- TODO: size check in safe mode
  --hdr <- getObjectHeader buf
  --size <- mkAnd hdr (ConstI64 0xffffffff)
  offset <- unboxInt' offsetObj
  payloadStart <- getObjectPayloadAddr {t=I8} buf
  bytePtr <- getElementPtr payloadStart offset
  strLength <- mkZext {to=I64} !(getStringByteLength valObj)
  strPayload <- getObjectPayloadAddr {t=I8} valObj
  appendCode $ "  call void @llvm.memcpy.p1i8.p1i8.i64(" ++ toIR bytePtr ++ ", " ++ toIR strPayload ++ ", " ++ toIR strLength ++ ", i1 false)"
  mkUnit

mk_prim__bufferCopyData : Vect 6 (IRValue IRObjPtr) -> Codegen (IRValue IRObjPtr)
mk_prim__bufferCopyData [src, startObj, lenObj, dest, locObj, _] = do
  start <- unboxInt' startObj
  len <- unboxInt' lenObj
  srcPayloadStart <- getObjectPayloadAddr {t=I8} src
  srcPtr <- getElementPtr srcPayloadStart start

  loc <- unboxInt' locObj
  dstPayloadStart <- getObjectPayloadAddr {t=I8} dest
  dstPtr <- getElementPtr dstPayloadStart loc

  appendCode $ "  call void @llvm.memmove.p1i8.p1i8.i64(" ++ toIR dstPtr ++ ", " ++ toIR srcPtr ++ ", " ++ toIR len ++ ", i1 false)"
  mkUnit

mk_prim__nullAnyPtr : Vect 1 (IRValue IRObjPtr) -> Codegen (IRValue IRObjPtr)
mk_prim__nullAnyPtr [p] = do
  lblStart <- genLabel "nullAnyPtr_start"
  lblInside <- genLabel "nullAnyPtr_inside"
  lblEnd <- genLabel "nullAnyPtr_end"

  jump lblStart
  beginLabel lblStart

  ptrObjIsZero <- SSA I1 <$> assignSSA ("call fastcc i1 @rapid.ptrisnull(" ++ toIR p ++ ")")
  branch ptrObjIsZero lblEnd lblInside

  beginLabel lblInside
  payload <- getObjectSlot {t=I64} p 0
  payloadIsZero <- icmp "eq" (ConstI64 0) payload

  jump lblEnd

  beginLabel lblEnd
  isNullPtr <- phi [(ptrObjIsZero, lblStart), (payloadIsZero, lblInside)]
  result <- cgMkInt !(mkZext isNullPtr)
  pure result

mk_prim__getString : Vect 1 (IRValue IRObjPtr) -> Codegen (IRValue IRObjPtr)
mk_prim__getString [p] = do
  assertObjectType' p OBJECT_TYPE_ID_POINTER
  payload <- getObjectSlot {t=IRObjPtr} p 0
  assertObjectType' payload OBJECT_TYPE_ID_STR
  pure payload

mk_prim__noop2 : Vect 2 (IRValue IRObjPtr) -> Codegen (IRValue IRObjPtr)
mk_prim__noop2 [_, _] = do
  mkUnit

mk_prim__currentDir : Vect 1 (IRValue IRObjPtr) -> Codegen (IRValue IRObjPtr)
mk_prim__currentDir [_] = do
  dummy <- mkStr "/tmp"
  newPtr <- dynamicAllocate (Const I64 8)
  putObjectHeader newPtr (constHeader OBJECT_TYPE_ID_POINTER 0)
  putObjectSlot newPtr (Const I64 0) dummy
  pure newPtr

mk_prelude_fastPack : Vect 1 (IRValue IRObjPtr) -> Codegen (IRValue IRObjPtr)
mk_prelude_fastPack [charListObj] = do
  newObj <- foreignCall {t=IRObjPtr} "@rapid_fast_pack" [toIR charListObj]
  pure newObj

mk_prelude_fastAppend : Vect 1 (IRValue IRObjPtr) -> Codegen (IRValue IRObjPtr)
mk_prelude_fastAppend [stringListObj] = do
  newObj <- foreignCall {t=IRObjPtr} "@rapid_fast_append" [toIR stringListObj]
  pure newObj

TAG_LIST_NIL : IRValue I32
TAG_LIST_NIL = Const I32 0
TAG_LIST_CONS : IRValue I32
TAG_LIST_CONS = Const I32 1

mk_prelude_fastUnpack : Vect 1 (IRValue IRObjPtr) -> Codegen (IRValue IRObjPtr)
mk_prelude_fastUnpack [strObj] = do
  nilHdr <- mkHeader OBJECT_TYPE_ID_CON_NO_ARGS TAG_LIST_NIL
  nilObj <- dynamicAllocate (Const I64 0)
  putObjectHeader nilObj nilHdr

  startLbl <- genLabel "start"
  returnLbl <- genLabel "ret"
  loopInitLbl <- genLabel "li"
  loopStartLbl <- genLabel "ls"
  loopBodyLbl <- genLabel "ls"
  loopEndLbl <- genLabel "le"

  jump startLbl
  beginLabel startLbl

  stringByteLength <- getStringByteLength strObj
  isEmpty <- icmp "eq" stringByteLength (Const I32 0)
  branch isEmpty returnLbl loopInitLbl

  beginLabel loopInitLbl
  resultObj <- dynamicAllocate (Const I64 16)
  putObjectHeader resultObj !(mkHeader (OBJECT_TYPE_ID_CON_NO_ARGS + 0x200) TAG_LIST_CONS)
  payload0 <- getObjectPayloadAddr {t=I8} strObj
  jump loopStartLbl

  beginLabel loopStartLbl
  nextBytePos <- SSA I32 <$> mkVarName "%nI."
  nextTail <- SSA IRObjPtr <$> mkVarName "%nT."
  bytePos <- phi [((Const I32 0), loopInitLbl), (nextBytePos, loopBodyLbl)]
  currentTail <- phi [(resultObj, loopInitLbl), (nextTail, loopBodyLbl)]

  payload <- getElementPtr payload0 bytePos
  decodedRaw <- call {t=I64} "ccc" "@utf8_decode1_length" [toIR payload]
  charVal <- mkTrunc {to=I32} decodedRaw
  decodedLength <- mkTrunc {to=I32} !(mkShiftR decodedRaw (Const I64 32))
  ch <- cgMkChar charVal
  putObjectSlot currentTail (Const I64 0) ch

  appendCode $ (showWithoutType nextBytePos) ++ " = add " ++ toIR bytePos ++ ", " ++ showWithoutType decodedLength

  finished <- icmp "uge" nextBytePos stringByteLength
  branch finished loopEndLbl loopBodyLbl

  beginLabel loopBodyLbl

  gc <- gcFlavour <$> getOpts
  dynamicAllocateInto gc (showWithoutType nextTail) (Const I64 16)
  putObjectHeader nextTail !(mkHeader (OBJECT_TYPE_ID_CON_NO_ARGS + 0x200) TAG_LIST_CONS)
  putObjectSlot currentTail (Const I64 1) nextTail

  jump loopStartLbl

  beginLabel loopEndLbl
  putObjectSlot currentTail (Const I64 1) nilObj
  jump returnLbl

  beginLabel returnLbl
  phi [(nilObj, startLbl), (resultObj, loopEndLbl)]

TAG_UNCONS_RESULT_EOF : IRValue I32
TAG_UNCONS_RESULT_EOF = Const I32 0
TAG_UNCONS_RESULT_CHARACTER : IRValue I32
TAG_UNCONS_RESULT_CHARACTER = Const I32 1

mk_prim__stringIteratorNew : Vect 1 (IRValue IRObjPtr) -> Codegen (IRValue IRObjPtr)
mk_prim__stringIteratorNew [strObj] = do
  iterObj <- cgMkInt (Const I64 0)
  pure iterObj

mk_prim__stringIteratorNext : Vect 2 (IRValue IRObjPtr) -> Codegen (IRValue IRObjPtr)
mk_prim__stringIteratorNext [strObj, iteratorObj] = do
  offset <- unboxInt' iteratorObj
  strLength <- mkZext !(getStringByteLength strObj)
  mkIf (icmp "uge" offset strLength) (do
       eofObj <- dynamicAllocate (Const I64 0)
       hdr <- mkHeader OBJECT_TYPE_ID_CON_NO_ARGS TAG_UNCONS_RESULT_EOF
       putObjectHeader eofObj hdr
       pure eofObj
    ) (do
       resultObj <- dynamicAllocate (Const I64 16)
       hdrWithoutSize <- mkHeader OBJECT_TYPE_ID_CON_NO_ARGS TAG_UNCONS_RESULT_CHARACTER
       hdr <- mkOr hdrWithoutSize (Const I64 (2 `prim__shl_Integer` 40))
       putObjectHeader resultObj hdr

       payload0 <- getObjectPayloadAddr {t=I8} strObj
       payload <- getElementPtr payload0 offset

       decodedRaw <- call {t=I64} "ccc" "@utf8_decode1_length" [toIR payload]
       charVal <- mkTrunc {to=I32} decodedRaw
       decodedLength <- mkTrunc {to=I32} !(mkShiftR decodedRaw (Const I64 32))

       charObj <- cgMkChar charVal
       putObjectSlot resultObj (Const I64 0) charObj

       newOffset <- mkAdd !(mkZext decodedLength) offset
       newIter <- cgMkInt newOffset
       putObjectSlot resultObj (Const I64 1) newIter
       pure resultObj
    )

-- Needs to be kept in sync with time.c:
CLOCK_TYPE_UTC : Int
CLOCK_TYPE_UTC = 1
CLOCK_TYPE_MONOTONIC : Int
CLOCK_TYPE_MONOTONIC = 2
CLOCK_TYPE_DURATION : Int
CLOCK_TYPE_DURATION = 3
CLOCK_TYPE_PROCESS : Int
CLOCK_TYPE_PROCESS = 4
CLOCK_TYPE_THREAD : Int
CLOCK_TYPE_THREAD = 5
CLOCK_TYPE_GCCPU : Int
CLOCK_TYPE_GCCPU = 6
CLOCK_TYPE_GCREAL : Int
CLOCK_TYPE_GCREAL = 7

mk_prim__readTime : Int -> Vect 1 (IRValue IRObjPtr) -> Codegen (IRValue IRObjPtr)
mk_prim__readTime clockType [_] = do
  clock <- dynamicAllocate (Const I64 16)
  hdr <- mkHeader OBJECT_TYPE_ID_CLOCK (Const I32 0)
  putObjectHeader clock hdr

  r <- foreignCall {t=I32} "@rapid_clock_read" [toIR clock, "i32 " ++ show clockType]

  pure clock

mk_prim__clockSecond : Vect 2 (IRValue IRObjPtr) -> Codegen (IRValue IRObjPtr)
mk_prim__clockSecond [clockObj, _] = do
  secondsAddr <- getObjectSlotAddrVar {t=I64} clockObj (Const I64 0)
  seconds <- cgMkBits64 !(load secondsAddr)
  pure seconds

mk_prim__clockNanoSecond : Vect 2 (IRValue IRObjPtr) -> Codegen (IRValue IRObjPtr)
mk_prim__clockNanoSecond [clockObj, _] = do
  nanoSecondsAddr <- getObjectSlotAddrVar {t=I64} clockObj (Const I64 1)
  nanoSeconds <- cgMkBits64 !(load nanoSecondsAddr)
  pure nanoSeconds

mk_prim__clockIsValid : Vect 2 (IRValue IRObjPtr) -> Codegen (IRValue IRObjPtr)
mk_prim__clockIsValid [clockObj, _] = do
  -- clock objects store either "1" or "0" in the size field as validity flag
  valid <- cgMkInt !(mkZext !(getObjectSize clockObj))
  pure valid

export
builtinPrimitives : List (String, (n : Nat ** (Vect n (IRValue IRObjPtr) -> Codegen (IRValue IRObjPtr))))
builtinPrimitives = [
    ("prim/blodwen-new-buffer", (2 ** mk_prim__bufferNew))
  , ("prim/blodwen-buffer-size", (1 ** mk_prim__bufferSize))
  , ("prim/blodwen-buffer-setbyte", (4 ** mk_prim__bufferSetByte))
  , ("prim/blodwen-buffer-getbyte", (3 ** mk_prim__bufferGetByte))
  , ("prim/blodwen-buffer-setbits16", (4 ** mk_prim__bufferSetBits16))
  , ("prim/blodwen-buffer-getbits16", (3 ** mk_prim__bufferGetBits16))
  , ("prim/blodwen-buffer-setint32", (4 ** mk_prim__bufferSetInt32))
  , ("prim/blodwen-buffer-getint32", (3 ** mk_prim__bufferGetInt32))
  , ("prim/blodwen-buffer-setint", (4 ** mk_prim__bufferSetInt))
  , ("prim/blodwen-buffer-getint", (3 ** mk_prim__bufferGetInt))
  , ("prim/blodwen-buffer-setdouble", (4 ** mk_prim__bufferSetDouble))
  , ("prim/blodwen-buffer-getdouble", (3 ** mk_prim__bufferGetDouble))
  , ("prim/blodwen-buffer-setstring", (4 ** mk_prim__bufferSetString))
  , ("prim/blodwen-buffer-getstring", (4 ** mk_prim__bufferGetString))
  , ("prim/blodwen-buffer-copydata", (6 ** mk_prim__bufferCopyData))

  , ("prim/string-concat", (1 ** mk_prelude_fastAppend))
  , ("prim/string-pack", (1 ** mk_prelude_fastPack))
  , ("prim/string-unpack", (1 ** mk_prelude_fastUnpack))

  , ("prim/blodwen-string-iterator-new", (1 ** mk_prim__stringIteratorNew))
  , ("prim/blodwen-string-iterator-next", (2 ** mk_prim__stringIteratorNext))
  --, ("prim/blodwen-string-iterator-to-string", (4 ** mk_prim__stringIteratorToString))

  , ("prim/blodwen-clock-time-utc", (1 ** mk_prim__readTime CLOCK_TYPE_UTC))
  , ("prim/blodwen-clock-time-monotonic", (1 ** mk_prim__readTime CLOCK_TYPE_MONOTONIC))
  , ("prim/blodwen-clock-time-duration", (1 ** mk_prim__readTime CLOCK_TYPE_DURATION))
  , ("prim/blodwen-clock-time-process", (1 ** mk_prim__readTime CLOCK_TYPE_PROCESS))
  , ("prim/blodwen-clock-time-thread", (1 ** mk_prim__readTime CLOCK_TYPE_THREAD))
  , ("prim/blodwen-clock-time-gccpu", (1 ** mk_prim__readTime CLOCK_TYPE_GCCPU))
  , ("prim/blodwen-clock-time-gcreal", (1 ** mk_prim__readTime CLOCK_TYPE_GCREAL))

  , ("prim/blodwen-clock-second", (2 ** mk_prim__clockSecond))
  , ("prim/blodwen-clock-nanosecond", (2 ** mk_prim__clockNanoSecond))
  , ("prim/blodwen-is-time", (2 ** mk_prim__clockIsValid))

  , ("prim/isNull", (1 ** mk_prim__nullAnyPtr))
  , ("prim/getString", (1 ** mk_prim__getString))
  , ("prim/noop2", (2 ** mk_prim__noop2))
  ]

compileExtPrimFallback : Name -> List (IRValue IRObjPtr) -> Codegen (IRValue IRObjPtr)
compileExtPrimFallback n args =
  do hp <- ((++) "%RuntimePtr ") <$> assignSSA "load %RuntimePtr, %RuntimePtr* %HpVar"
     hpLim <- ((++) "%RuntimePtr ") <$> assignSSA "load %RuntimePtr, %RuntimePtr* %HpLimVar"
     let base = "%TSOPtr %BaseArg"
     result <- assignSSA $ "call fastcc %Return1 @_extprim_" ++ (safeName n) ++ "(" ++ showSep ", " (hp::base::hpLim::(map toIR args)) ++ ")"

     newHp <- assignSSA $ "extractvalue %Return1 " ++ result ++ ", 0"
     appendCode $ "store %RuntimePtr " ++ newHp ++ ", %RuntimePtr* %HpVar"
     newHpLim <- assignSSA $ "extractvalue %Return1 " ++ result ++ ", 1"
     appendCode $ "store %RuntimePtr " ++ newHpLim ++ ", %RuntimePtr* %HpLimVar"
     returnValue <- SSA IRObjPtr <$> assignSSA ("extractvalue %Return1 " ++ result ++ ", 2")
     pure returnValue

export
compileExtPrim : Name -> List (IRValue IRObjPtr) -> Codegen (IRValue IRObjPtr)
compileExtPrim (NS ns n) args with (unsafeUnfoldNamespace ns)
  compileExtPrim (NS ns (UN $ Basic "prim__newArray")) [_, countArg, elemArg, _] | ["Prims", "IOArray", "Data"] = do
    lblStart <- genLabel "new_array_init_start"
    lblLoop <- genLabel "new_array_init_loop"
    lblEnd <- genLabel "new_array_init_end"
    count <- unboxInt' countArg
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
    store elemArg addr

    appendCode $ iPlus1name ++ " = add " ++ toIR i ++ ", 1"
    continue <- icmp "ult" iPlus1 count
    branch continue lblLoop lblEnd
    beginLabel lblEnd
    pure newObj

  compileExtPrim (NS ns (UN $ Basic "prim__arrayGet")) [_, array, indexArg, _] | ["Prims", "IOArray", "Data"] = do
    index <- unboxInt' indexArg
    addr <- getObjectSlotAddrVar array index
    load addr


  compileExtPrim (NS ns (UN $ Basic "prim__arraySet")) [_, array, indexArg, val, _] | ["Prims", "IOArray", "Data"] = do
    index <- unboxInt' indexArg
    addr <- getObjectSlotAddrVar array index
    store val addr
    mkUnit

  compileExtPrim (NS ns (UN $ Basic "prim__codegen")) [] | ["Info", "System"] = do
    mkStr "rapid"
  compileExtPrim (NS ns (UN $ Basic "prim__os")) [] | ["Info", "System"] = do
    -- no cross compiling for now:
    mkStr System.Info.os
  compileExtPrim (NS ns (UN $ Basic "void")) _ | ["Uninhabited", "Prelude"] = do
    appendCode $ "  call ccc void @rapid_crash(i8* bitcast ([23 x i8]* @error_msg_void to i8*)) noreturn"
    appendCode $ "unreachable"
    pure nullPtr
  compileExtPrim (NS ns (UN $ Basic "prim__void")) _ | ["Uninhabited", "Prelude"] = do
    appendCode $ "  call ccc void @rapid_crash(i8* bitcast ([23 x i8]* @error_msg_void to i8*)) noreturn"
    appendCode $ "unreachable"
    pure nullPtr
  compileExtPrim (NS ns (UN $ Basic "prim__newIORef")) [_, val, _] | ["IORef", "Data"] = do
    ioRefObj <- dynamicAllocate (Const I64 8)
    putObjectHeader ioRefObj !(mkHeader OBJECT_TYPE_ID_IOREF (Const I32 0))
    putObjectSlot ioRefObj (Const I64 0) val
    pure ioRefObj
  compileExtPrim (NS ns (UN $ Basic "prim__readIORef")) [_, ioRefObj, _] | ["IORef", "Data"] = do
    getObjectSlot ioRefObj 0
  compileExtPrim (NS ns (UN $ Basic "prim__writeIORef")) [_, ioRefObj, payload, _] | ["IORef", "Data"] = do
    putObjectSlot ioRefObj (Const I64 0) payload
    mkUnit
  compileExtPrim (NS ns n) args | _ = compileExtPrimFallback (NS ns n) args
compileExtPrim n args = compileExtPrimFallback n args
