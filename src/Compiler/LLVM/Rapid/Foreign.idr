module Compiler.LLVM.Rapid.Foreign

import Data.List

import Compiler.CompileExpr
import Compiler.VMCode
import Core.Name

import Compiler.LLVM.IR
import Compiler.LLVM.Instruction
import Compiler.LLVM.Rapid.Object
import Control.Codegen
import Data.Utils
import Rapid.Common

-- we provide our own in Data.Utils
%hide Core.Name.Namespace.showSep

fromCFType : CFType -> IRType
fromCFType CFChar = I32
fromCFType (CFIORes CFChar) = I32
fromCFType CFInt = I64
fromCFType (CFIORes CFInt) = I64
fromCFType CFDouble = F64
fromCFType (CFIORes CFDouble) = F64
fromCFType _ = IRObjPtr

cftypeIsUnit : CFType -> Bool
cftypeIsUnit CFUnit = True
cftypeIsUnit (CFIORes CFUnit) = True
cftypeIsUnit _ = False

wrapForeignResult : (cft : CFType) -> (v: IRValue (fromCFType cft)) -> Codegen (IRValue IRObjPtr)
wrapForeignResult (CFChar) v = cgMkChar v
wrapForeignResult (CFIORes CFChar) v = cgMkChar v
wrapForeignResult (CFInt) v = cgMkInt v
wrapForeignResult (CFIORes CFInt) v = cgMkInt v
wrapForeignResult (CFDouble) v = cgMkDouble v
wrapForeignResult (CFIORes CFDouble) v = cgMkDouble v
wrapForeignResult _ (SSA _ v) = pure (SSA IRObjPtr v)
wrapForeignResult _ _ = do
  addError "can not wrap foreign result"
  pure (SSA IRObjPtr "error")

||| Call a "runtime-aware" foreign function, i.e. one, that can interact with the RTS
export
foreignCall : {t : IRType} -> String -> List String -> Codegen (IRValue t)
foreignCall {t} name args = do
  hp <- load globalHpVar
  hpLim <- load globalHpLimVar
  baseHpPointer <- SSA (Pointer 0 RuntimePtr) <$> assignSSA ("getelementptr inbounds %Idris_TSO.struct, %TSOPtr %BaseArg, i32 0, i32 1")
  store hp baseHpPointer
  result <- SSA t <$> (assignSSA $ "  call ccc " ++ show t ++ " " ++ name ++ "(" ++ (showSep ", " ("%TSOPtr %BaseArg"::args)) ++ ")")
  store !(load baseHpPointer) globalHpVar
  pure result

export
foreignVoidCall : String -> List String -> Codegen ()
foreignVoidCall name args = do
  hp <- load globalHpVar
  hpLim <- load globalHpLimVar
  baseHpPointer <- SSA (Pointer 0 RuntimePtr) <$> assignSSA ("getelementptr inbounds %Idris_TSO.struct, %TSOPtr %BaseArg, i32 0, i32 1")
  store hp baseHpPointer
  appendCode $ "  call ccc void " ++ name ++ "(" ++ (showSep ", " ("%TSOPtr %BaseArg"::args)) ++ ")"
  store !(load baseHpPointer) globalHpVar

export
prepareArgCallConv' : List String -> List String
prepareArgCallConv' rest = ["%RuntimePtr %HpArg", "%TSOPtr %BaseArg", "%RuntimePtr %HpLimArg"] ++ rest

export
prepareArgCallConv : List String -> List String
prepareArgCallConv l = prepareArgCallConv' l

transformArg : (IRValue IRObjPtr, CFType) -> Codegen String
transformArg (arg, CFChar) = do
  i <- unboxChar' arg
  pure (toIR i)
transformArg (arg, CFInt) = do
  i <- unboxInt' arg
  pure (toIR i)
transformArg (arg, CFDouble) = do
  d <- unboxFloat64' arg
  pure (toIR d)
transformArg (arg, _) = pure (toIR arg)

-- TODO: in this file, reg2val is only ever used with RVal, refactor
rvalVar : IRValue (Pointer 0 IRObjPtr)
rvalVar = SSA (Pointer 0 IRObjPtr) ("%rvalVar")


export
genericForeign : String -> Name -> (argTypes : List CFType) -> CFType -> Codegen ()
genericForeign foreignName name argTypes ret = do
  let args = map (\(i, _) => SSA IRObjPtr ("%arg" ++ show i)) (enumerate argTypes)
  appendCode ("define private fastcc %Return1 @" ++ safeName name ++ "(" ++ (showSep ", " $ prepareArgCallConv $ map toIR args) ++ ") gc \"statepoint-example\" {")
  funcEntry
  if cftypeIsUnit ret then do
    foreignVoidCall ("@" ++ foreignName) !(traverse transformArg (zip args argTypes))
    store !(mkUnit) rvalVar
    else do
      fgResult <- foreignCall {t=fromCFType ret} ("@" ++ foreignName) !(traverse transformArg (zip args argTypes))
      store !(wrapForeignResult ret fgResult) rvalVar
  funcReturn
  appendCode "\n}\n"

export
missingForeign : List String -> Name -> (argTypes : List CFType) -> Codegen ()
missingForeign cs name argTypes = do
  let args = map (\(i, _) => SSA IRObjPtr ("%arg" ++ show i)) (enumerate argTypes)
  appendCode ("define private fastcc %Return1 @" ++ safeName name ++ "(" ++ (showSep ", " $ prepareArgCallConv $ map toIR args) ++ ") gc \"statepoint-example\" {")
  funcEntry
  appendCode $ "call ccc void @idris_rts_crash(i64 404) noreturn"
  addError $ "missing foreign: " ++ show name ++ " <- " ++ show cs
  funcReturn
  appendCode "\n}\n"

