module Compiler.LLVM.Rapid.Object

import Data.Vect

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
getObjectSize : IRValue IRObjPtr -> Codegen (IRValue I32)
getObjectSize obj = do
  hdr <- getObjectHeader obj
  mkTrunc {to=I32} hdr

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

export
cgMkChar : IRValue I32 -> Codegen (IRValue IRObjPtr)
cgMkChar val = do
  newObj <- dynamicAllocate (ConstI64 0)
  header <- mkHeader OBJECT_TYPE_ID_CHAR val
  putObjectHeader newObj header
  pure newObj

export
unboxChar' : IRValue IRObjPtr -> Codegen (IRValue I32)
unboxChar' src = do
  charHdr <- getObjectHeader src
  pure !(mkTrunc charHdr)

export
mkCon : Int -> List (IRValue IRObjPtr) -> Codegen (IRValue IRObjPtr)
mkCon tag args = do
  newObj <- dynamicAllocate (ConstI64 $ cast (8 * (length args)))
  -- TODO: add object type to header for GC
  hdr <- mkHeader OBJECT_TYPE_ID_CON_NO_ARGS (pConst tag)
  hdrWithArgCount <- mkOr hdr (Const I64 ((cast $ length args) `prim__shl_Integer` 40))
  putObjectHeader newObj hdrWithArgCount
  let enumArgs = enumerate args
  for_ enumArgs (\x => let (i, arg) = x in do
                            assertObjectTypeAny arg (cast i+1)
                            putObjectSlot newObj (ConstI64 $ cast i) arg
                          )
  pure newObj

export
mkConstCon : Int -> List (String) -> Codegen (IRValue IRObjPtr)
mkConstCon tag args = do
  let newHeader = constHeader (OBJECT_TYPE_ID_CON_NO_ARGS + (256 * (cast $ length args))) (cast tag)
  let typeSignature = "{i64" ++ repeatStr ", %ObjPtr" (length args) ++ "}"
  cName <- addConstant $ "private unnamed_addr addrspace(1) constant " ++ typeSignature ++ " {" ++ toIR newHeader ++ (concat $ map ((++) ", ") args) ++ "}, align 8"
  pure $ SSA IRObjPtr $ "bitcast (" ++ typeSignature ++ " addrspace(1)* " ++ cName ++ " to %ObjPtr)"

export
cgMkDouble : IRValue F64 -> Codegen (IRValue IRObjPtr)
cgMkDouble val = do
  newObj <- dynamicAllocate (ConstI64 8)
  putObjectHeader newObj (constHeader OBJECT_TYPE_ID_DOUBLE 0)
  putObjectSlot newObj (ConstI64 0) val
  pure newObj

export
cgMkConstDouble : Double -> Codegen (IRValue IRObjPtr)
cgMkConstDouble d = do
  let newHeader = constHeader OBJECT_TYPE_ID_DOUBLE 0
  let typeSignature = "{i64, double}"
  cName <- addConstant $ "private unnamed_addr addrspace(1) constant " ++ typeSignature ++ " {" ++ toIR newHeader ++ ", double 0x" ++ (assert_total $ doubleToHex d) ++ "}, align 8"
  pure $ SSA IRObjPtr $ "bitcast (" ++ typeSignature ++ " addrspace(1)* " ++ cName ++ " to %ObjPtr)"

export
cgMkDoubleFromBits : IRValue I64 -> Codegen (IRValue IRObjPtr)
cgMkDoubleFromBits val = do
  newObj <- dynamicAllocate (ConstI64 8)
  putObjectHeader newObj (constHeader OBJECT_TYPE_ID_DOUBLE 0)
  putObjectSlot newObj (Const I64 0) val
  pure newObj

export
unboxFloat64 : IRValue (Pointer 0 IRObjPtr) -> Codegen (IRValue F64)
unboxFloat64 src = getObjectSlot {t=F64} !(load src) 0

export
unboxFloat64' : IRValue IRObjPtr -> Codegen (IRValue F64)
unboxFloat64' src = getObjectSlot {t=F64} src 0

export
cgMkInt : IRValue I64 -> Codegen (IRValue IRObjPtr)
cgMkInt val = do
  boxed <- assignSSA $ "tail call fastcc noalias %ObjPtr @llvm.rapid.boxint(" ++ toIR val ++ ") \"gc-leaf-function\""
  pure (SSA IRObjPtr boxed)

export
unboxInt' : IRValue IRObjPtr -> Codegen (IRValue I64)
unboxInt' src = SSA I64 <$> assignSSA ("tail call fastcc i64 @llvm.rapid.unboxint(" ++ toIR src ++ ") \"gc-leaf-function\"")

export
cgMkBits64 : IRValue I64 -> Codegen (IRValue IRObjPtr)
cgMkBits64 val = do
  newObj <- dynamicAllocate (ConstI64 8)
  putObjectHeader newObj (constHeader OBJECT_TYPE_ID_BITS64 0)
  putObjectSlot newObj (ConstI64 0) val
  pure newObj

-- TODO: change to List Bits8
utf8EncodeChar : Char -> List Int
utf8EncodeChar c = let codepoint = cast {to=Int} c
                       bor = prim__or_Int
                       band = prim__and_Int
                       shr = prim__shr_Int in
                       map id $
                       if codepoint <= 0x7f then [codepoint]
                       else if codepoint <= 0x7ff then [
                         bor 0xc0 (codepoint `shr` 6),
                         bor 0x80 (codepoint `band` 0x3f)
                         ]
                       else if codepoint <= 0xffff then [
                         bor 0xe0 (codepoint `shr` 12),
                         bor 0x80 ((codepoint `shr` 6) `band` 0x3f),
                         bor 0x80 ((codepoint `shr` 0) `band` 0x3f)
                         ]
                       else [
                         bor 0xf0 (codepoint `shr` 18),
                         bor 0x80 ((codepoint `shr` 12) `band` 0x3f),
                         bor 0x80 ((codepoint `shr` 6) `band` 0x3f),
                         bor 0x80 ((codepoint `shr` 0) `band` 0x3f)
                         ]

utf8EncodeString : String -> List Int
utf8EncodeString s = concatMap utf8EncodeChar $ unpack s

getStringIR : List Int -> String
getStringIR utf8bytes = concatMap okchar utf8bytes
  where
    okchar : Int -> String
    -- c >= ' ' && c <= '~' && c /= '\\' && c /= '"'
    okchar c = if c >= 32 && c <= 126 && c /= 92 && c /= 34
                  then cast $ cast {to=Char} c
                  else "\\" ++ asHex2 c

export
mkStr : String -> Codegen (IRValue IRObjPtr)
mkStr s = do
  let utf8bytes = utf8EncodeString s
  let len = length utf8bytes
  let newHeader = constHeader OBJECT_TYPE_ID_STR (cast len)
  let typeSignature = "{i64, [" ++ show len ++ " x i8]}"
  cName <- addConstant $ "private unnamed_addr addrspace(1) constant " ++ typeSignature ++ " {" ++ toIR newHeader ++ ", [" ++ show len ++ " x i8] c\"" ++ (getStringIR utf8bytes) ++ "\"}, align 8"
  pure $ SSA IRObjPtr $ "bitcast (" ++ typeSignature ++ " addrspace(1)* " ++ cName ++ " to %ObjPtr)"

export
getStringByteLength : IRValue IRObjPtr -> Codegen (IRValue I32)
getStringByteLength = getObjectSize

export
mkUnit : Codegen (IRValue IRObjPtr)
mkUnit = mkCon 0 []

{- Runtime-related stuff, might fit into own module -}
export
mkRuntimeCrash : String -> Codegen ()
mkRuntimeCrash s = do
  msg <- mkStr s
  appendCode $ "  call ccc void @idris_rts_crash_msg(" ++ toIR msg ++ ") noreturn"
  appendCode $ "unreachable"

export
globalHpVar : IRValue (Pointer 0 RuntimePtr)
globalHpVar = SSA (Pointer 0 RuntimePtr) "%HpVar"

export
globalHpLimVar : IRValue (Pointer 0 RuntimePtr)
globalHpLimVar = SSA (Pointer 0 RuntimePtr) "%HpLimVar"

export
globalRValVar : IRValue (Pointer 0 IRObjPtr)
globalRValVar = SSA (Pointer 0 IRObjPtr) "%rvalVar"

export
funcEntry : Codegen ()
funcEntry = do
  appendCode "%HpVar = alloca %RuntimePtr\n"
  appendCode "%HpLimVar = alloca %RuntimePtr\n"
  appendCode "%rvalVar = alloca %ObjPtr\n"
  store (SSA RuntimePtr "%HpArg") globalHpVar
  store (SSA RuntimePtr "%HpLimArg") globalHpLimVar
  store nullPtr globalRValVar

export
funcReturn : Codegen ()
funcReturn = do
  finHp <- load globalHpVar
  finHpLim <- load globalHpLimVar
  finRVal <- load globalRValVar

  ret1 <- assignSSA $ "insertvalue %Return1 undef, " ++ toIR finHp ++ ", 0"
  ret2 <- assignSSA $ "insertvalue %Return1 " ++ ret1 ++ ", " ++ toIR finHpLim ++ ", 1"
  ret3 <- assignSSA $ "insertvalue %Return1 " ++ ret2 ++ ", " ++ toIR finRVal ++ ", 2"
  appendCode $ "ret %Return1 " ++ ret3
