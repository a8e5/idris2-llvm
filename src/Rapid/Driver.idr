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

gcPreamble : GCFlavour -> String
gcPreamble gc =
  ("@rapid_gc_flavour = constant i32 " ++ (show $ encodeGCFlavourAsInt gc) ++ "\n")
    ++ if (gc /= Statepoint) then "@_LLVM_StackMaps = constant [1 x i8] [i8 0]\n@__LLVM_StackMaps = constant [1 x i8] [i8 0]\n"
                             else ""

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
