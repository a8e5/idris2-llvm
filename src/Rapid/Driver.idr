module Rapid.Driver

import Data.List
import Data.String
import System
import System.File

import Compiler.Common
import Compiler.VMCode
import Core.Directory
import Core.CompileExpr
import Core.Name
import Libraries.Data.SortedMap
import Libraries.Utils.Path

import Rapid.Common
import Compiler.GenLLVMIR
import Compiler.LLVM.IR
import Compiler.PrepareCode

gcStubsBDW : String
gcStubsBDW =
  """
  @_LLVM_StackMaps = constant [1 x i8] [i8 0]\n@__LLVM_StackMaps = constant [1 x i8] [i8 0]
  declare ccc %ObjPtr @GC_malloc(i64)
  declare ccc %ObjPtr @GC_malloc_atomic(i64)
  define external i8* @get_stackmap() { call ccc void @idris_rts_crash(i64 76) noreturn \n unreachable }
  \n
  """

gcStubs : CompileOpts -> String
gcStubs (MkCompileOpts _ _ Statepoint targetOs) =
  """
  define external ccc void @GC_init() { call ccc void @idris_rts_crash(i64 76) noreturn \n unreachable }
  define external ccc void @GC_disable() { call ccc void @idris_rts_crash(i64 76) noreturn \n unreachable }
  define external ccc i8* @GC_malloc() { call ccc void @idris_rts_crash(i64 76) noreturn \n unreachable }
  \n
  """ ++
  llvmStackmapsSymbolName ++ " = external global i8\n"
  ++ "define external i8* @get_stackmap() { ret i8* " ++ llvmStackmapsSymbolName ++ "}\n"
    where llvmStackmapsSymbolName : String
          llvmStackmapsSymbolName =
            if targetOs == "darwin"
               -- on Apple platforms, an underscore is added implicitly:
               then "@_LLVM_StackMaps"
               else "@__LLVM_StackMaps"
gcStubs (MkCompileOpts _ _ BDW _) = gcStubsBDW
gcStubs (MkCompileOpts _ _ Zero _) = gcStubsBDW

gcPreamble : CompileOpts -> String
gcPreamble opts =
  ("@rapid_gc_flavour = constant i32 " ++ (show $ encodeGCFlavourAsInt $ gcFlavour opts) ++ "\n")
    ++ gcStubs opts

export
writeIR : (functions : List (Name, VMDef)) ->
          (support : String) -> (outfile : String) -> (opts : CompileOpts) -> IO ()
writeIR functions support outfile opts = do
  let nameMap = getNameMap $ map snd functions
  let indexedFuncs = enumerate functions
  let fcount = length indexedFuncs
  ignore $ fPutStrLn stderr $ "functions to compile: " ++ show (length indexedFuncs)
  (Right outFile) <- openFile outfile WriteTruncate
  | Left err => putStrLn $ "error opening output file: " ++ show err
  ignore $ fPutStr outFile support
  ignore $ fPutStr outFile (gcPreamble opts)
  ignore $ fPutStr outFile (closureHelper opts)

  for_ indexedFuncs (\c => do
    -- This dummy IO forces the `getVMIR` calls to wait until it's "their turn",
    -- otherwise we get huge memory spikes.
    ignore $ fPutStr stderr ""
    let i = 1 + fst c
    when (i `mod` 100 == 0) $ ignore $ fPutStrLn stderr ("compile fun " ++ show i ++ "/" ++ (show fcount) ++ ": " ++ safeName (fst (snd c)))
    let funcIr = getVMIR opts nameMap c
    fPutStr outFile funcIr
    )
  closeFile outFile
