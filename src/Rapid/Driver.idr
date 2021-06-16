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
import Compiler.PrepareCode

gcStubs : GCFlavour -> String
gcStubs Statepoint =
  """
  define external ccc void @GC_init() { call ccc void @idris_rts_crash(i64 76) noreturn \n unreachable }
  define external ccc void @GC_disable() { call ccc void @idris_rts_crash(i64 76) noreturn \n unreachable }
  define external ccc i8* @GC_malloc() { call ccc void @idris_rts_crash(i64 76) noreturn \n unreachable }

  ; we use 2 weak symbols instead of just the right one, to avoid
  ; having to generate target-dependent IR
  @__LLVM_StackMaps = extern_weak global i8
  ; on Apple platforms, an underscore is added implicitly:
  @_LLVM_StackMaps = extern_weak global i8
  define external i8* @get_stackmap() {
    ; check which reference is non-null and return that one
    %d = ptrtoint i8* @__LLVM_StackMaps to i64
    %ok = icmp ne i64 %d, 0
    %r = select i1 %ok, i8* @__LLVM_StackMaps, i8* @_LLVM_StackMaps
    ret i8* %r
  }
  \n
  """
gcStubs BDW =
  """
  @_LLVM_StackMaps = constant [1 x i8] [i8 0]\n@__LLVM_StackMaps = constant [1 x i8] [i8 0]
  declare ccc %ObjPtr @GC_malloc(i64)
  declare ccc %ObjPtr @GC_malloc_atomic(i64)
  define external i8* @get_stackmap() { call ccc void @idris_rts_crash(i64 76) noreturn \n unreachable }
  \n
  """
gcStubs Zero = gcStubs BDW

gcPreamble : GCFlavour -> String
gcPreamble gc =
  ("@rapid_gc_flavour = constant i32 " ++ (show $ encodeGCFlavourAsInt gc) ++ "\n")
    ++ gcStubs gc

export
writeIR : (functions : List (Name, VMDef)) -> (foreigns : List (Name, NamedDef)) ->
          (support : String) -> (outfile : String) -> (opts : CompileOpts) -> IO ()
writeIR functions foreigns support outfile opts = do
  let foreignCode = map (compileForeign opts) (enumerate foreigns)
  let nameMap = getNameMap $ map snd functions
  let indexedFuncs = enumerate functions
  let fcount = length indexedFuncs
  ignore $ fPutStrLn stderr $ "functions to compile: " ++ show (length indexedFuncs)
  (Right outFile) <- openFile outfile WriteTruncate
  | Left err => putStrLn $ "error opening output file: " ++ show err
  ignore $ fPutStr outFile support
  ignore $ fPutStr outFile (gcPreamble $ gcFlavour opts)
  ignore $ fPutStr outFile (closureHelper opts)
  ignore $ fPutStr outFile $ fastAppend foreignCode

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
