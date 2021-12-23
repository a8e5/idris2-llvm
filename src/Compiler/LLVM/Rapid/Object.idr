module Compiler.LLVM.Rapid.Object

import Control.Codegen
import Compiler.LLVM.IR
import Compiler.LLVM.Instruction
import Data.Utils
import Rapid.Common

export
OBJECT_TYPE_ID_CON_NO_ARGS : Int
OBJECT_TYPE_ID_CON_NO_ARGS = 0xff

export
OBJECT_TYPE_ID_INT : Int
OBJECT_TYPE_ID_INT = 1

-- for now treat all numbers the same
export
OBJECT_TYPE_ID_DOUBLE : Int
OBJECT_TYPE_ID_DOUBLE = 1

export
OBJECT_TYPE_ID_BITS64 : Int
OBJECT_TYPE_ID_BITS64 = 1

export
OBJECT_TYPE_ID_STR : Int
OBJECT_TYPE_ID_STR = 2

export
OBJECT_TYPE_ID_CLOSURE : Int
OBJECT_TYPE_ID_CLOSURE = 3

export
OBJECT_TYPE_ID_CHAR : Int
OBJECT_TYPE_ID_CHAR = 4

export
OBJECT_TYPE_ID_IOREF : Int
OBJECT_TYPE_ID_IOREF = 5

export
OBJECT_TYPE_ID_BUFFER : Int
OBJECT_TYPE_ID_BUFFER = 6

export
OBJECT_TYPE_ID_OPAQUE : Int
OBJECT_TYPE_ID_OPAQUE = 0x07

export
OBJECT_TYPE_ID_POINTER : Int
OBJECT_TYPE_ID_POINTER = 0x08

export
OBJECT_TYPE_ID_IOARRAY : Int
OBJECT_TYPE_ID_IOARRAY = 0x09

export
OBJECT_TYPE_ID_BIGINT : Int
OBJECT_TYPE_ID_BIGINT = 0x0a

export
OBJECT_TYPE_ID_CLOCK : Int
OBJECT_TYPE_ID_CLOCK = 0x0b

export
getObjectSlotAddrVar : {t : IRType} -> IRValue IRObjPtr -> IRValue I64 -> Codegen (IRValue (Pointer 1 t))
getObjectSlotAddrVar obj pos = do
  slotPtr <- SSA (Pointer 1 $ Pointer 0 I8) <$> assignSSA ("getelementptr inbounds %Object, " ++ toIR obj ++ ", i32 0, i32 1, " ++ toIR pos)
  bitcastA slotPtr

export
getObjectPayloadAddr : {t : IRType} -> IRValue IRObjPtr -> Codegen (IRValue (Pointer 1 t))
getObjectPayloadAddr obj = getObjectSlotAddrVar obj (Const I64 0)

export
getObjectSlot : {t : IRType} -> IRValue IRObjPtr -> Int -> Codegen (IRValue t)
getObjectSlot obj n = load !(getObjectSlotAddrVar obj (Const I64 $ cast n))

export
putObjectSlot : {t : IRType} -> IRValue IRObjPtr -> IRValue I64 -> IRValue t -> Codegen ()
putObjectSlot {t} obj pos val = store val !(getObjectSlotAddrVar obj pos)

export
getObjectHeader : IRValue IRObjPtr -> Codegen (IRValue I64)
getObjectHeader obj = do
  headerPtr <- SSA (Pointer 1 I64) <$> assignSSA ("getelementptr inbounds %Object, " ++ (toIR obj) ++ ", i32 0, i32 0")
  load headerPtr

export
putObjectHeader : IRValue IRObjPtr -> IRValue I64 -> Codegen ()
putObjectHeader obj hdr = do
  headerPtr <- SSA (Pointer 1 I64) <$> assignSSA ("getelementptr inbounds %Object, " ++ (toIR obj) ++ ", i32 0, i32 0")
  store hdr headerPtr

export
mkHeader : Int -> IRValue I32 -> Codegen (IRValue I64)
mkHeader objType sizeOrTag =
  mkOr (Const I64 $ (cast objType) `prim__shl_Integer` 32) !(mkZext sizeOrTag)

export
constHeader : Int -> Bits32 -> IRValue I64
constHeader objType sizeOrTag =
  Const I64 $ cast (((cast objType) `prim__shl_Bits64` 32) `prim__or_Bits64` (cast sizeOrTag))

HEADER_SIZE : IRValue I64
HEADER_SIZE = (Const I64 8)

export
dynamicAllocateInto : GCFlavour -> String -> IRValue I64 -> Codegen ()
dynamicAllocateInto Statepoint destVarName payloadSize = do
  totalSize <- mkAddNoWrap payloadSize HEADER_SIZE

  hp <- ((++) "%RuntimePtr ") <$> assignSSA "load %RuntimePtr, %RuntimePtr* %HpVar"
  hpLim <- ((++) "%RuntimePtr ") <$> assignSSA "load %RuntimePtr, %RuntimePtr* %HpLimVar"
  let base = "%TSOPtr %BaseArg"

  allocated <- assignSSA $ "call fastcc %Return1 @rapid_allocate_fast(" ++ showSep ", " [hp, base, hpLim] ++ ", "++(toIR totalSize)++") alwaysinline optsize nounwind"
  newHp <- assignSSA $ "extractvalue %Return1 " ++ allocated ++ ", 0"
  appendCode $ "store %RuntimePtr " ++ newHp ++ ", %RuntimePtr* %HpVar"
  newHpLim <- assignSSA $ "extractvalue %Return1 " ++ allocated ++ ", 1"
  appendCode $ "store %RuntimePtr " ++ newHpLim ++ ", %RuntimePtr* %HpLimVar"
  appendCode $ destVarName ++ " = extractvalue %Return1 " ++ allocated ++ ", 2"
dynamicAllocateInto Zero destVarName payloadSize = do
  payloadSizePlus7 <- mkAddNoWrap payloadSize (Const I64 7)
  payloadSizeAligned <- mkAnd payloadSizePlus7 (Const I64 (-8))
  totalSize <- mkAddNoWrap payloadSizeAligned HEADER_SIZE
  appendCode $ destVarName ++ " = call ccc %ObjPtr @malloc(" ++ toIR totalSize ++ ")"
dynamicAllocateInto BDW destVarName payloadSize = do
  payloadSizePlus7 <- mkAddNoWrap payloadSize (Const I64 7)
  payloadSizeAligned <- mkAnd payloadSizePlus7 (Const I64 (-8))
  totalSize <- mkAddNoWrap payloadSizeAligned HEADER_SIZE
  appendCode $ destVarName ++ " = call ccc %ObjPtr @GC_malloc(" ++ toIR totalSize ++ ")"

export
dynamicAllocate : IRValue I64 -> Codegen (IRValue IRObjPtr)
dynamicAllocate payloadSize = do
  varName <- mkVarName "%a"
  gc <- gcFlavour <$> getOpts
  dynamicAllocateInto gc varName payloadSize
  pure $ SSA IRObjPtr varName

TRACE : Bool
TRACE = False

export
assertObjectTypeAny : IRValue IRObjPtr -> Integer -> Codegen ()
assertObjectTypeAny o msg = when TRACE $ do
  let tVal = (Const I64 (0x10000 + msg))
  typeOk <- genLabel "typecheck_ok"
  typeError <- genLabel "typecheck_error"
  typeEnd <- genLabel "typecheck_end"

  hdr <- getObjectHeader o
  hdrTypFull <- mkShiftR hdr (Const I64 32)
  hdrTyp <- mkAnd (Const I64 0xff) hdrTypFull
  hdrTypOk <- icmp "ne" (Const I64 0) hdrTyp
  branch hdrTypOk typeOk typeError
  beginLabel typeError
  appendCode $ "call ccc void @idris_rts_crash_typecheck(" ++ showSep ", " [toIR o, toIR tVal] ++ ") noreturn"
  appendCode $ "unreachable"
  beginLabel typeOk

export
assertObjectType' : IRValue IRObjPtr -> Int -> Codegen ()
assertObjectType' o t = when TRACE $ do
  let tVal = (Const I64 $ cast t)
  typeOk <- genLabel "typecheck_ok"
  typeError <- genLabel "typecheck_error"
  typeEnd <- genLabel "typecheck_end"

  hdr <- getObjectHeader o
  hdrTypFull <- mkShiftR hdr (Const I64 32)
  hdrTyp <- mkAnd (Const I64 0xff) hdrTypFull
  hdrTypOk <- icmp "eq" tVal hdrTyp
  branch hdrTypOk typeOk typeError
  beginLabel typeError
  appendCode $ "call ccc void @idris_rts_crash_typecheck(" ++ showSep ", " [toIR o, toIR tVal] ++ ") noreturn"
  appendCode $ "unreachable"
  beginLabel typeOk
