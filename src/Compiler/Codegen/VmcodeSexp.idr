module Compiler.Codegen.VmcodeSexp

import Data.List
import Data.String
import System
import System.File

import Core.Directory
import Core.CompileExpr
import Core.Context
import Compiler.Common
import Compiler.VMCode
import Libraries.Utils.Path

import Data.Sexp
import Compiler.VMCodeSexp

dumpDef : (Name, VMDef) -> String
dumpDef d = (show $ toSexp d) ++ "\n\n"

compile : Ref Ctxt Defs -> (tmpDir : String) -> (outputDir : String) ->
        ClosedTerm -> (outfile : String) -> Core (Maybe String)
compile defs tmpDir outputDir term outfile = do
  let appDirRel = outfile ++ "_rapid" -- relative to build dir
  let appDirGen = outputDir </> appDirRel -- relative to here
  let outputFileName = appDirGen </> (outfile ++ ".sexp") -- VMCode S-exp
  Right () <- coreLift $ mkdirAll appDirGen
    | Left err => do coreLift_ $ putStrLn $ "ERROR" ++ show err
                     pure Nothing

  cd <- getCompileData False VMCode term
  let compiledFunctions = map dumpDef (vmcode cd)

  coreLift $ do
    (Right outFile) <- openFile outputFileName WriteTruncate
    | Left err => putStrLn $ "error opening output file: " ++ show err
    for_ compiledFunctions (fPutStr outFile)
    ignore $ closeFile outFile
    pure ()

  pure $ Nothing

execute : Ref Ctxt Defs -> (tmpDir : String) -> ClosedTerm -> Core ()
execute defs tmpDir term = do coreLift $ putStrLn "Can't execute VMCode directly"

export
vmcodeSexp : Codegen
vmcodeSexp = MkCG compile execute Nothing Nothing
