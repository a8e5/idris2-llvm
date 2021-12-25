module Compiler.LLVM.Rapid.Closure

import Core.Name

import Compiler.LLVM.IR
import Compiler.LLVM.Instruction
import Compiler.LLVM.Rapid.Object
import Control.Codegen
import Data.Utils

-- A "fat" closure is always invoked via its "closure entry" function
export
FAT_CLOSURE_LIMIT : Int
FAT_CLOSURE_LIMIT = 8

CLOSURE_MAX_ARGS : Int
CLOSURE_MAX_ARGS = 1024

export
mkClosure : Name -> Nat -> List (IRValue IRObjPtr) -> Codegen (IRValue IRObjPtr)
mkClosure n missingN [] =
  pure $ SSA IRObjPtr $ "bitcast ({i64, %FuncPtr} addrspace(1)* @\{safeName n}$$closureNoArgs to %ObjPtr)"

mkClosure n missingN args = do
  let missing = cast {to=Int} missingN
  let len = cast {to=Int} $ length args
  let totalArgsExpected = missing + len
  if totalArgsExpected > (cast CLOSURE_MAX_ARGS)
     then do
       addError $ "ERROR : too many closure arguments: " ++ show totalArgsExpected ++ " > " ++ show CLOSURE_MAX_ARGS
       pure nullPtr
     else do
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
      let (i, argObj) = iv
      putObjectSlot newObj (Const I64 $ cast $ i+1) argObj
      pure ()
      )
  pure newObj
