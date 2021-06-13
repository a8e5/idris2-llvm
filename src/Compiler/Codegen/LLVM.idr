module Compiler.Codegen.LLVM

import Data.List
import Data.String
import System
import System.File

import Core.Directory
import Core.CompileExpr
import Core.Context
import Core.Options
import Compiler.Common
import Compiler.VMCode
import Libraries.Utils.Path

import Rapid.Common
import Rapid.Driver

isFgn : (Name, a, NamedDef) -> Bool
isFgn (_, _, (MkNmForeign _ _ _)) = True
isFgn _ = False

shell : List String -> String
shell args = showSep " " $ map shellQuote args
  where
    shellQuote : String -> String
    shellQuote s = s

runShell : List String -> IO ()
runShell args = do
  let cmd = shell args
  ignore $ fPutStrLn stderr $ "+" ++ cmd
  rc <- system cmd
  case rc of
       0 => pure ()
       err_rc => do putStrLn $ "command failed with exit code " ++ show err_rc
                    exitFailure

globalizeStackmap : String -> IO Bool
globalizeStackmap fname = do
  (Right outFile) <- openFile fname Append
  | Left err => do putStrLn $ "error opening asm file: " ++ show err
                   pure False
  ignore $ fPutStr outFile "\n.globl __LLVM_StackMaps\n"
  ignore $ closeFile outFile
  pure True

getDebug : List String -> Bool
getDebug = any isDebug
  where
    isDebug : String -> Bool
    isDebug directive = (trim directive) == "debug"

getGCDirective : List String -> Either String GCFlavour
getGCDirective [] = pure Statepoint
getGCDirective ("gc=zero"::_) = pure Zero
getGCDirective ("gc=bdw"::_) = pure BDW
getGCDirective ("gc=statepoint"::_) = pure Statepoint
getGCDirective (x::xs) = if isPrefixOf "gc=" x
  then Left ("invalid GC flavour: " ++ x ++ " (supported: zero,bdw,statepoint)")
  else getGCDirective xs

compile : Ref Ctxt Defs -> (tmpDir : String) -> (outputDir : String) ->
        ClosedTerm -> (outfile : String) -> Core (Maybe String)
compile defs tmpDir outputDir term outfile = do
  let appDirRel = outfile ++ "_rapid" -- relative to build dir
  let appDirGen = outputDir </> appDirRel -- relative to here
  let outputFileName = appDirGen </> (outfile ++ ".ll") -- LLVM IR (text)
  let bcFileName = appDirGen </> (outfile ++ ".bc") -- optimized LLVM bitcode
  let asmFileName = appDirGen </> (outfile ++ ".s") -- compiled assembler
  let objectFileName = appDirGen </> (outfile ++ ".o") -- object file
  let binaryFileName = outputDir </> outfile
  coreLift_ $ mkdirAll appDirGen

  directives <- getDirectives (Other "llvm")
  let debug = getDebug directives
  coreLift_ $ fPutStrLn stderr ("debug: " ++ show debug)

  (Right gc) <- pure $ getGCDirective directives
    | Left err => (coreLift_ $ fPutStrLn stderr err) >> (pure Nothing)
  coreLift_ $ fPutStrLn stderr ("selected GC strategy: " ++ show gc)

  -- load supporting files first, so we can fail early
  support <- readDataFile $ "rapid" </> "support.ll"
  runtime <- findDataFile $ "rapid" </> "runtime.bc"
  platformLib <- findDataFile $ "rapid" </> "platform.a"
  rapidLLVMPlugin <- findDataFile $ "rapid" </> "librapid.so"

  cd <- getCompileData False VMCode term
  coreLift_ $ fPutStrLn stderr $ "got compiledata"
  let foreigns = map (\(n,_,d) => (n,d)) $ filter isFgn $ namedDefs cd
  let allFunctions = vmcode cd
  coreLift_ $ writeIR allFunctions foreigns support outputFileName debug

  let initPasses = [
    "mem2reg", "instsimplify", "constmerge", "sccp", "dce", "globaldce"
    ]
  let optPasses = []
  let lateTransformPasses = ["rewrite-statepoints-for-gc", "rapid-lower"]
  let passes = concat $ intersperse "," (initPasses ++ optPasses ++ lateTransformPasses)
  coreLift $ do
    runShell $ [
      "opt",
      "-load-pass-plugin=" ++ rapidLLVMPlugin,
      "--passes=" ++ passes,
      "-o=" ++ bcFileName,
      outputFileName
      ]
    runShell ["llc", "--frame-pointer=all", "-tailcallopt", "-o=" ++ asmFileName, bcFileName]
    True <- globalizeStackmap asmFileName
    | False => putStrLn "error"
    runShell ["clang", "-c", "-o", objectFileName, asmFileName]
    runShell ["clang", "-o", binaryFileName, objectFileName, runtime, platformLib, "-lm", "-L/usr/local/lib", "-lgmp"]

    pure ()

  pure $ Nothing

execute : Ref Ctxt Defs -> (tmpDir : String) -> ClosedTerm -> Core ()
execute defs tmpDir term = do
  let tmpExecutableFile = "idris_tmp"
  ignore $ compile defs tmpDir tmpDir term tmpExecutableFile
  coreLift_ $ system (tmpDir </> tmpExecutableFile)
  pure ()

export
rapidCodegen : Codegen
rapidCodegen = MkCG compile execute
