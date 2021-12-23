module Compiler.LLVM.Instruction

import Data.Vect

import Control.Codegen
import Compiler.LLVM.IR
import Data.Utils

-- TODO: get rid of this instance, replace usage with lower-level calls
export
ToIR String where
  toIR = id
  showWithoutType = id

export
load : {t : IRType} -> IRValue (Pointer n t) -> Codegen (IRValue t)
load IRDiscard = pure nullPtr
load {t} mv = do
  loaded <- assignSSA $ "load " ++ (show t) ++ ", " ++ (toIR mv)
  pure $ SSA t loaded

export
store : {t : IRType} -> IRValue t -> IRValue (Pointer n t) -> Codegen ()
store _ IRDiscard = pure ()
store {t} v dst = do
  appendCode $ "  store " ++ (toIR v) ++ ", " ++ (toIR dst)

export
icmp : {t : IRType} -> String -> IRValue t -> IRValue t -> Codegen (IRValue I1)
icmp {t} cond a b = do
  compare <- assignSSA $ "icmp " ++ cond ++ " " ++ (show t) ++ " " ++ showWithoutType a ++ ", " ++ showWithoutType b
  pure $ SSA I1 compare

export
fcmp : String -> IRValue F64 -> IRValue F64 -> Codegen (IRValue I1)
fcmp cond a b = do
  compare <- assignSSA $ "fcmp " ++ cond ++ " double " ++ showWithoutType a ++ ", " ++ showWithoutType b
  pure $ SSA I1 compare

export
mkZext : {to : IRType} -> IRValue from -> Codegen (IRValue to)
mkZext {to} val = (SSA to) <$> assignSSA ("zext " ++ toIR val ++ " to " ++ show to)

export
mkSext : {to : IRType} -> IRValue from -> Codegen (IRValue to)
mkSext {to} val = (SSA to) <$> assignSSA ("sext " ++ toIR val ++ " to " ++ show to)

export
fptosi : {to : IRType} -> IRValue from -> Codegen (IRValue to)
fptosi {to} val = (SSA to) <$> assignSSA ("fptosi " ++ toIR val ++ " to " ++ show to)

export
fptoui : {to : IRType} -> IRValue from -> Codegen (IRValue to)
fptoui {to} val = (SSA to) <$> assignSSA ("fptoui " ++ toIR val ++ " to " ++ show to)

export
sitofp : {to : IRType} -> IRValue from -> Codegen (IRValue to)
sitofp {to} val = (SSA to) <$> assignSSA ("sitofp " ++ toIR val ++ " to " ++ show to)

export
uitofp : {to : IRType} -> IRValue from -> Codegen (IRValue to)
uitofp {to} val = (SSA to) <$> assignSSA ("uitofp " ++ toIR val ++ " to " ++ show to)

export
phi : {t : IRType} -> List (IRValue t, IRLabel) -> Codegen (IRValue t)
phi xs = (SSA t) <$> assignSSA ("phi " ++ show t ++ " " ++ showSep ", " (map getEdge xs)) where
  getEdge : (IRValue t, IRLabel) -> String
  getEdge (val, lbl) = "[ " ++ showWithoutType val ++ ", " ++ showWithoutType lbl ++ " ]"

export
getElementPtr : {t : IRType} -> {n : Int} -> IRValue (Pointer n t) -> IRValue ot -> Codegen (IRValue (Pointer n t))
getElementPtr {t} {n} ptr offset =
  SSA (Pointer n t) <$> assignSSA ("getelementptr inbounds " ++ show t ++ ", " ++ toIR ptr ++ ", " ++ toIR offset)

export
bitcastA : {from : IRType} -> {to : IRType} -> {n : Int} -> IRValue from -> Codegen (IRValue (Pointer n to))
bitcastA {from} {to} {n} val = (SSA (Pointer n to)) <$> assignSSA ("bitcast " ++ toIR val ++ " to " ++ show (Pointer n to))

export
mkBinOp : {t : IRType} -> String -> IRValue t -> IRValue t -> Codegen (IRValue t)
mkBinOp {t} s a b = do
  result <- assignSSA $ s ++ " " ++ (show t) ++ " " ++ showWithoutType a ++ ", " ++ showWithoutType b
  pure $ SSA t result

export
mkXOr : {t : IRType} -> IRValue t -> IRValue t -> Codegen (IRValue t)
mkXOr = mkBinOp "xor"

export
mkOr : {t : IRType} -> IRValue t -> IRValue t -> Codegen (IRValue t)
mkOr = mkBinOp "or"

export
mkAnd : {t : IRType} -> IRValue t -> IRValue t -> Codegen (IRValue t)
mkAnd = mkBinOp "and"

export
mkAdd : {t : IRType} -> IRValue t -> IRValue t -> Codegen (IRValue t)
mkAdd = mkBinOp "add"

export
mkAddNoWrap : {t : IRType} -> IRValue t -> IRValue t -> Codegen (IRValue t)
mkAddNoWrap = mkBinOp "add nuw nsw"

export
mkMul : {t : IRType} -> IRValue t -> IRValue t -> Codegen (IRValue t)
mkMul = mkBinOp "mul"

export
mkSub : {t : IRType} -> IRValue t -> IRValue t -> Codegen (IRValue t)
mkSub = mkBinOp "sub"

export
mkSDiv : {t : IRType} -> IRValue t -> IRValue t -> Codegen (IRValue t)
mkSDiv = mkBinOp "sdiv"

export
mkSRem : {t : IRType} -> IRValue t -> IRValue t -> Codegen (IRValue t)
mkSRem = mkBinOp "srem"

export
mkUDiv : {t : IRType} -> IRValue t -> IRValue t -> Codegen (IRValue t)
mkUDiv = mkBinOp "udiv"

export
mkURem : {t : IRType} -> IRValue t -> IRValue t -> Codegen (IRValue t)
mkURem = mkBinOp "urem"

export
mkShiftL : {t : IRType} -> IRValue t -> IRValue t -> Codegen (IRValue t)
mkShiftL = mkBinOp "shl"

export
mkShiftR : {t : IRType} -> IRValue t -> IRValue t -> Codegen (IRValue t)
mkShiftR = mkBinOp "lshr"

export
mkAShiftR : {t : IRType} -> IRValue t -> IRValue t -> Codegen (IRValue t)
mkAShiftR = mkBinOp "ashr"

export
unlikely : IRValue I1 -> Codegen (IRValue I1)
unlikely cond = (SSA I1) <$> assignSSA (" call ccc i1 @llvm.expect.i1(" ++ toIR cond ++ ", i1 0)")

export
likely : IRValue I1 -> Codegen (IRValue I1)
likely cond = (SSA I1) <$> assignSSA (" call ccc i1 @llvm.expect.i1(" ++ toIR cond ++ ", i1 1)")

export
branch : IRValue I1 -> (true : IRLabel) -> (false : IRLabel) -> Codegen ()
branch cond whenTrue whenFalse =
  appendCode $ "br " ++ toIR cond ++ ", " ++ toIR whenTrue ++ ", " ++ toIR whenFalse

export
jump : IRLabel -> Codegen ()
jump to =
  appendCode $ "br " ++ toIR to

export
mkSelect : {t : IRType} -> IRValue I1 -> IRValue t -> IRValue t -> Codegen (IRValue t)
mkSelect {t} s a b = do
  (SSA t) <$> assignSSA ("select " ++ toIR s ++ ", " ++ toIR a ++ ", " ++ toIR b)

export
mkMin : {t : IRType} -> IRValue t -> IRValue t -> Codegen (IRValue t)
mkMin {t} a b = do
  aSmaller <- icmp "slt" a b
  (SSA t) <$> assignSSA ("select " ++ toIR aSmaller ++ ", " ++ toIR a ++ ", " ++ toIR b)

export
mkMax : {t : IRType} -> IRValue t -> IRValue t -> Codegen (IRValue t)
mkMax {t} a b = do
  aLarger <- icmp "sgt" a b
  (SSA t) <$> assignSSA ("select " ++ toIR aLarger ++ ", " ++ toIR a ++ ", " ++ toIR b)

export
voidCall : String -> String -> Vect n String -> Codegen ()
voidCall cconv name args =
  appendCode $ "  call " ++ cconv ++ " void " ++ name ++ "(" ++ (showSep ", " (toList args)) ++ ")"

export
call : {t : IRType} -> String -> String -> Vect n String -> Codegen (IRValue t)
call {t} cconv name args =
  SSA t <$> (assignSSA $ "  call " ++ cconv ++ " " ++ show t ++ " " ++ name ++ "(" ++ (showSep ", " (toList args)) ++ ")")

export
mkTrunc : {to : IRType} -> IRValue from -> Codegen (IRValue to)
mkTrunc {to} val = (SSA to) <$> assignSSA ("trunc " ++ toIR val ++ " to " ++ show to)

export
mkAbs : IRValue I32 -> Codegen (IRValue I32)
mkAbs val = call "ccc" "@rapid.abs.i32" [toIR val, "i1 1"]

export
mkAbs64 : IRValue I64 -> Codegen (IRValue I64)
mkAbs64 val = call "ccc" "@rapid.abs.i64" [toIR val, "i1 1"]
