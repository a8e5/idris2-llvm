module Compiler.LLVM.Rapid.Foreign

import Data.List
import Data.String
import Data.Vect

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

export
builtinForeign : (n : Nat ** (Vect n (IRValue IRObjPtr) -> Codegen (IRValue IRObjPtr))) -> Name -> (argTypes : List CFType) -> CFType -> Codegen ()
builtinForeign builtin name argTypes ret = do
  let (n ** f) = builtin
  appendCode ("define external fastcc %Return1 @" ++ safeName name ++ "(" ++ (showSep ", " $ prepareArgCallConv $ toList $ map toIR (args n)) ++ ") gc \"statepoint-example\" {")
  funcEntry
  result <- f (args n)
  store result $ SSA (Pointer 0 IRObjPtr) ("%rvalVar")
  funcReturn
  appendCode "\n}\n"
  where
  args : (n : Nat) -> Vect n (IRValue IRObjPtr)
  args n = map (\i => SSA IRObjPtr $ "%arg" ++ show (finToNat i)) range

foreignRedirectMap : List (String, String)
foreignRedirectMap = [
    ("C:idris2_openFile, libidris2_support, idris_file.h", "rapid_system_file_open")
  , ("C:fdopen,libc 6", "rapid_system_fdopen")
  , ("C:idris2_closeFile, libidris2_support, idris_file.h", "rapid_system_file_close")
  , ("C:fflush,libc 6", "rapid_system_file_flush")
  , ("C:idris2_fileSize, libidris2_support, idris_file.h", "rapid_system_file_size")
  , ("C:idris2_fileAccessTime, libidris2_support, idris_file.h", "rapid_system_file_atime")
  , ("C:idris2_fileStatusTime, libidris2_support, idris_file.h", "rapid_system_file_ctime")
  , ("C:idris2_fileModifiedTime, libidris2_support, idris_file.h", "rapid_system_file_mtime")
  , ("C:idris2_readLine, libidris2_support, idris_file.h", "rapid_system_file_read_line")
  , ("C:idris2_readChars, libidris2_support, idris_file.h", "rapid_system_file_read_chars")
  , ("C:idris2_seekLine, libidris2_support, idris_file.h", "rapid_system_file_seek_line")
  , ("C:fgetc,libc 6", "rapid_system_file_read_char")
  , ("C:idris2_chmod, libidris2_support, idris_file.h", "rapid_system_file_chmod")
  , ("C:getchar,libc 6", "rapid_system_getchar")
  , ("C:putchar,libc 6", "rapid_system_putchar")
  , ("C:idris2_getStr, libidris2_support, idris_support.h", "rapid_system_stdin_getline")
  , ("C:idris2_writeLine, libidris2_support, idris_file.h", "rapid_system_file_write_string")
  , ("C:idris2_eof, libidris2_support, idris_file.h", "rapid_system_file_eof")
  , ("C:idris2_removeFile, libidris2_support, idris_file.h", "rapid_system_file_remove")
  , ("C:idris2_fileError, libidris2_support, idris_file.h", "rapid_system_file_error")
  , ("C:idris2_fileErrno, libidris2_support, idris_file.h", "rapid_system_errno")
  , ("C:idris2_getErrno, libidris2_support, idris_support.h", "rapid_system_errno")
  , ("C:idris2_stdin, libidris2_support, idris_file.h", "rapid_system_file_stdin")
  , ("C:idris2_stdout, libidris2_support, idris_file.h", "rapid_system_file_stdout")
  , ("C:idris2_stderr, libidris2_support, idris_file.h", "rapid_system_file_stderr")
  , ("C:idris2_currentDirectory, libidris2_support, idris_directory.h", "rapid_system_current_dir")
  , ("C:idris2_createDir, libidris2_support, idris_directory.h", "rapid_system_dir_create")
  , ("C:idris2_changeDir, libidris2_support, idris_directory.h", "rapid_system_dir_change")
  , ("C:idris2_removeDir, libidris2_support, idris_directory.h", "rapid_system_dir_remove")
  , ("C:idris2_openDir, libidris2_support, idris_directory.h", "rapid_system_dir_open")
  , ("C:idris2_closeDir, libidris2_support, idris_directory.h", "rapid_system_dir_close")
  , ("C:idris2_nextDirEntry, libidris2_support, idris_directory.h", "rapid_system_dir_next_entry")
  , ("C:idris2_popen, libidris2_support, idris_file.h", "rapid_system_popen")
  , ("C:idris2_pclose, libidris2_support, idris_file.h", "rapid_system_pclose")
  , ("C:idris2_free, libidris2_support, idris_memory.h", "rapid_system_free")
  , ("C:idris2_putStr, libidris2_support, idris_support.h", "rapid_putstr")
  , ("C:idris2_readBufferData, libidris2_support, idris_file.h", "idris_rts_read_buffer_data")
  , ("C:idris2_writeBufferData, libidris2_support, idris_file.h", "idris_rts_write_buffer_data")
  , ("C:idris2_isNull, libidris2_support, idris_support.h", "prim/isNull")
  , ("C:idris2_fileErrno, libidris2_suppor, idris_support.h", "rapid_system_file_errno")
  , ("C:idrnet_errno, libidris2_support, idris_net.h", "rapid_system_errno")
  , ("C:idris2_strerror, libidris2_support, idris_support.h", "rapid_system_strerror")
  , ("C:idris2_getString, libidris2_support, idris_support.h", "prim/getString")

  , ("C:idris2_setupTerm, libidris2_support, idris_term.h", "idris2_setupTerm")
  , ("C:idris2_getTermCols, libidris2_support, idris_term.h", "idris2_getTermCols")
  , ("C:idris2_getTermLines, libidris2_support, idris_term.h", "idris2_getTermLines")

  , ("scheme:blodwen-stringbytelen", "rapid_string_bytelength")
  , ("scheme:blodwen-string-iterator-new", "prim/blodwen-string-iterator-new")
  , ("scheme:blodwen-string-iterator-next", "prim/blodwen-string-iterator-next")
  , ("scheme:blodwen-string-iterator-to-string", "prim/blodwen-string-iterator-to-string")
  , ("C:exit, libc 6", "rapid_system_exit")
  , ("C:idris2_system, libidris2_support, idris_system.h", "rapid_system_system")
  , ("C:getenv, libc 6", "rapid_system_get_env")
  , ("scheme:blodwen-arg-count", "rapid_system_get_arg_count")
  , ("scheme:blodwen-arg", "rapid_system_get_arg")

  , ("C:idrnet_af_inet, libidris2_support, idris_net.h", "idrnet_af_inet")
  , ("C:idrnet_af_inet6, libidris2_support, idris_net.h", "idrnet_af_inet6")
  , ("C:idrnet_af_unix, libidris2_support, idris_net.h", "idrnet_af_unix")
  , ("C:idrnet_af_unspec, libidris2_support, idris_net.h", "idrnet_af_unspec")
  , ("C:idrnet_accept, libidris2_support, idris_net.h", "idrnet_accept")
  , ("C:idrnet_bind, libidris2_support, idris_net.h", "idrnet_bind")
  , ("C:idrnet_create_sockaddr, libidris2_support, idris_net.h", "idrnet_create_sockaddr")
  , ("C:idrnet_free, libidris2_support, idris_net.h", "idrnet_free")
  , ("C:idrnet_fdopen, libidris2_support, idris_net.h", "rapid_system_fdopen")
  , ("C:idrnet_sockaddr_family, libidris2_support, idris_net.h", "idrnet_sockaddr_family")
  , ("C:idrnet_sockaddr_ipv4, libidris2_support, idris_net.h", "idrnet_sockaddr_ipv4")
  , ("C:idrnet_sockaddr_unix, libidris2_support, idris_net.h", "idrnet_sockaddr_unix")
  , ("C:idrnet_socket, libidris2_support, idris_net.h", "idrnet_socket")
  , ("C:idrnet_listen, libidris2_support, idris_net.h", "idrnet_listen")

  , ("scheme:blodwen-buffer-size", "prim/blodwen-buffer-size")
  , ("scheme:blodwen-new-buffer", "prim/blodwen-new-buffer")
  , ("scheme:blodwen-buffer-free", "prim/noop2")
  , ("scheme:blodwen-buffer-setbyte", "prim/blodwen-buffer-setbyte")
  , ("scheme:blodwen-buffer-getbyte", "prim/blodwen-buffer-getbyte")
  , ("scheme:blodwen-buffer-setbits16", "prim/blodwen-buffer-setbits16")
  , ("scheme:blodwen-buffer-getbits16", "prim/blodwen-buffer-getbits16")
  , ("scheme:blodwen-buffer-setbits32", "prim/blodwen-buffer-setbits32")
  , ("scheme:blodwen-buffer-getbits32", "prim/blodwen-buffer-getbits32")
  , ("scheme:blodwen-buffer-setbits64", "prim/blodwen-buffer-setbits64")
  , ("scheme:blodwen-buffer-getbits64", "prim/blodwen-buffer-getbits64")
  , ("scheme:blodwen-buffer-setint32", "prim/blodwen-buffer-setint32")
  , ("scheme:blodwen-buffer-getint32", "prim/blodwen-buffer-getint32")
  , ("scheme:blodwen-buffer-setint", "prim/blodwen-buffer-setint")
  , ("scheme:blodwen-buffer-getint", "prim/blodwen-buffer-getint")
  , ("scheme:blodwen-buffer-setdouble", "prim/blodwen-buffer-setdouble")
  , ("scheme:blodwen-buffer-getdouble", "prim/blodwen-buffer-getdouble")
  , ("scheme:blodwen-buffer-setstring", "prim/blodwen-buffer-setstring")
  , ("scheme:blodwen-buffer-getstring", "prim/blodwen-buffer-getstring")
  , ("scheme:blodwen-buffer-copydata", "prim/blodwen-buffer-copydata")

  , ("scheme:blodwen-thread", "rapid_system_fork")

  , ("scheme:blodwen-clock-time-utc", "prim/blodwen-clock-time-utc")
  , ("scheme:blodwen-clock-time-monotonic", "prim/blodwen-clock-time-monotonic")
  , ("scheme:blodwen-clock-time-duration", "prim/blodwen-clock-time-duration")
  , ("scheme:blodwen-clock-time-process", "prim/blodwen-clock-time-process")
  , ("scheme:blodwen-clock-time-thread", "prim/blodwen-clock-time-thread")
  , ("scheme:blodwen-clock-time-gccpu", "prim/blodwen-clock-time-gccpu")
  , ("scheme:blodwen-clock-time-gcreal", "prim/blodwen-clock-time-gcreal")

  , ("scheme:blodwen-is-time?", "prim/blodwen-is-time")
  , ("scheme:blodwen-clock-second", "prim/blodwen-clock-second")
  , ("scheme:blodwen-clock-nanosecond", "prim/blodwen-clock-nanosecond")

  , ("scheme:string-concat", "prim/string-concat")
  , ("scheme:string-pack", "prim/string-pack")
  , ("scheme:string-unpack", "prim/string-unpack")
  ]

export
findForeignName : List String -> Maybe String
findForeignName cs =
  case find (isPrefixOf "rapid:") cs of
       Just found => Just (substr 6 99999 found)
       Nothing => choiceMap (\n => lookup n foreignRedirectMap) cs
