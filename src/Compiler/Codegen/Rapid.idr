module Compiler.Codegen.Rapid

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

  -- load supporting files first, so we can fail early
  support <- readDataFile $ "rapid" </> "support.ll"
  runtime <- findDataFile $ "rapid" </> "runtime.bc"
  platformLib <- findDataFile $ "rapid" </> "platform.a"
  rapidLLVMPlugin <- findDataFile $ "rapid" </> "librapid.so"

  cd <- getCompileData False VMCode term
  coreLift_ $ fPutStrLn stderr $ "got compiledata"
  let foreigns = map (\(n,_,d) => (n,d)) $ filter isFgn $ namedDefs cd
  let allFunctions = vmcode cd
  let optFlags = [
    "-mem2reg", "-instsimplify", "-constmerge", "-sccp", "-dce", "-globaldce",
    "-rewrite-statepoints-for-gc"]

  coreLift_ $ writeIR allFunctions foreigns support outputFileName debug

  let lateTransformFlags = ["-rapid-lower"]
  coreLift $ do
    runShell $ ["opt", outputFileName, "-load=" ++ rapidLLVMPlugin] ++ optFlags ++ lateTransformFlags ++ ["-o=" ++ bcFileName]
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
