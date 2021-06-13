module Control.Codegen

import public Control.Monad.State
import Data.List
import Data.String

import Debug.Trace
import Rapid.Common

ConstDef : Type
ConstDef = (String, String)

export
record CGBuffer where
  constructor MkCGBuf
  opts : CompileOpts
  i : Int
  consts : List ConstDef
  code : List String
  errors : List String

public export
Codegen : Type -> Type
Codegen = State CGBuffer

emptyCG : CompileOpts -> CGBuffer
emptyCG opts = MkCGBuf opts 0 [] [] []

export
getOpts : Codegen CompileOpts
getOpts = (.opts) <$> get

export
appendCode : String -> Codegen ()
appendCode c = modify $ record { code $= (c::)}

export
getUnique : Codegen Int
getUnique = do
  (MkCGBuf o i c l e) <- get
  put (MkCGBuf o (i+1) c l e)
  pure i

export
addConstant : Int -> String -> Codegen String
addConstant i v = do
  ci <- getUnique
  let name = "@glob_" ++ show i ++ "_c" ++ show ci
  (MkCGBuf o i c l e) <- get
  put (MkCGBuf o i ((name, v)::c) l e)
  pure name

export
addError : String -> Codegen ()
addError msg = do
  appendCode ("; ERROR: " ++ msg)
  (MkCGBuf o i c l e) <- get
  put $ trace ("add error: " ++ msg) (MkCGBuf o i c l (msg::e))

export
addMetadata : Int -> String -> Codegen String
addMetadata i v = do
  u <- getUnique
  let mdId = u * 0x10000 + i
  let name = "!" ++ show mdId
  (MkCGBuf o i c l e) <- get
  put (MkCGBuf o i ((name, v)::c) l e)
  pure name

export
runCodegen : CompileOpts -> Codegen () -> String
runCodegen o r = let (MkCGBuf _ _ cs ls errors) = fst $ runState (emptyCG o) r in
                     fastAppend $ intersperse "\n" $ (map (\(n,v) => n ++ " = " ++ v) $ reverse cs) ++ reverse ls
