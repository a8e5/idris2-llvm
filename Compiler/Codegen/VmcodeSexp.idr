module Compiler.Codegen.VmcodeSexp

import Data.List
import Data.Strings
import System
import System.File

import Core.Directory
import Core.CompileExpr
import Core.Context
import Compiler.Common
import Compiler.VMCode
import Utils.Path

import Data.Sexp
import Compiler.VMCodeSexp

dumpFgn : (Name, FC, NamedDef) -> String
dumpFgn (n, _, def@(MkNmForeign cs args ret)) = show (toSexp (n, def)) ++ "\n"
dumpFgn _ = "" -- not a foreign function

dumpDef : (Name, VMDef) -> String
dumpDef d = (show $ toSexp d) ++ "\n\n"

compile : Ref Ctxt Defs -> (tmpDir : String) -> (outputDir : String) ->
        ClosedTerm -> (outfile : String) -> Core (Maybe String)
compile defs tmpDir outputDir term outfile = do
  let appDirRel = outfile ++ "_rapid" -- relative to build dir
  let appDirGen = outputDir </> appDirRel -- relative to here
  let outputFileName = appDirGen </> (outfile ++ ".sexp") -- VMCode S-exp
  coreLift $ mkdirAll appDirGen

  cd <- getCompileData VMCode term
  let foreignDecls = map dumpFgn (namedDefs cd)
  let compiledFunctions = map dumpDef (vmcode cd)

  coreLift $ do
    (Right outFile) <- openFile outputFileName WriteTruncate
    | Left err => putStrLn $ "error opening output file: " ++ show err
    fPutStr outFile $ fastAppend foreignDecls
    for compiledFunctions (fPutStr outFile)
    closeFile outFile
    pure ()

  pure $ Nothing

execute : Ref Ctxt Defs -> (tmpDir : String) -> ClosedTerm -> Core ()
execute defs tmpDir term = do coreLift $ putStrLn "Can't execute VMCode directly"

export
vmcodeSexp : Codegen
vmcodeSexp = MkCG compile execute
