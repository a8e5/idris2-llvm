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
import Libraries.Utils.Path

import Compiler.GenLLVMIR
import Compiler.PrepareCode

export
writeIR : (functions : List (Name, VMDef)) -> (foreigns : List (Name, NamedDef)) ->
          (support : String) -> (outfile : String) -> (debug : Bool) -> IO ()
writeIR functions foreigns support outfile debug = do
  let foreignCode = map (compileForeign debug) (enumerate foreigns)
  let nameMap = getNameMap $ map snd functions
  let indexedFuncs = enumerate functions
  let fcount = length indexedFuncs
  ignore $ fPutStrLn stderr $ "functions to compile: " ++ show (length indexedFuncs)
  (Right outFile) <- openFile outfile WriteTruncate
  | Left err => putStrLn $ "error opening output file: " ++ show err
  ignore $ fPutStr outFile support
  ignore $ fPutStr outFile closureHelper
  ignore $ fPutStr outFile $ fastAppend foreignCode

  for_ indexedFuncs (\c => do
    let i = 1 + fst c
    when (i `mod` 100 == 0) $ ignore $ fPutStrLn stderr ("compile fun " ++ show i ++ "/" ++ (show fcount) ++ ": " ++ safeName (fst (snd c)))
    let funcIr = getVMIR debug nameMap c
    fPutStr outFile funcIr
    )
  closeFile outFile
