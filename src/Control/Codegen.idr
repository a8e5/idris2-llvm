module Control.Codegen

import public Control.Monad.State
import Data.List
import Data.String

import Libraries.Data.SortedMap

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
  constantValues : SortedMap Int String

public export
Codegen : Type -> Type
Codegen = State CGBuffer

emptyCG : CompileOpts -> CGBuffer
emptyCG opts = MkCGBuf opts 0 [] [] [] empty

export
getOpts : Codegen CompileOpts
getOpts = (.opts) <$> get

export
appendCode : String -> Codegen ()
appendCode c = modify { code $= (c::)}

export
getUnique : Codegen Int
getUnique = do
  st <- get
  let i = st.i
  put ({i := i+1} st)
  pure i

export
addConstant : String -> Codegen String
addConstant v = do
  ci <- getUnique
  st <- get
  let name = "@glob_" ++ show (st.opts.constNamespace) ++ "_c" ++ show ci
  put ({ consts $= ((name, v)::)} st)
  pure name

export
addError : String -> Codegen ()
addError msg = do
  appendCode ("; ERROR: " ++ msg)
  st <- get
  put $ trace ("add error: " ++ msg) ({errors $= (msg::)} st)

export
addMetadata : String -> Codegen String
addMetadata v = do
  i <- (.constNamespace) <$> getOpts
  u <- getUnique
  let mdId = u * 0x10000 + i
  let name = "!" ++ show mdId
  modify { consts $= ((name, v)::)}
  pure name

export
appendMetadata : String -> Codegen String
appendMetadata value = do
  o <- (.constNamespace) <$> getOpts
  i <- getUnique
  let varname = "!" ++ show (i * 1000000 + o)
  appendCode ("  " ++ varname ++ " = " ++ value)
  pure varname

export
trackValueConst : Int -> String -> Codegen ()
trackValueConst v c = do
  modify {constantValues $= insert v c}

export
removeValueConst : Int -> Codegen ()
removeValueConst v = do
  modify {constantValues $= delete v}

export
isValueConst : Int -> Codegen (Maybe String)
isValueConst v = do
  lookup v . (.constantValues) <$> get

export
forgetAllValuesConst : Codegen ()
forgetAllValuesConst = modify {constantValues := empty}

export
mkVarName : String -> Codegen String
mkVarName pfx = do
  i <- getUnique
  pure $ (pfx ++ show i)

export
runCodegen : CompileOpts -> Codegen () -> String
runCodegen o r = let (MkCGBuf _ _ cs ls errors _) = fst $ runState (emptyCG o) r in
                     fastConcat $ intersperse "\n" $ (map (\(n,v) => n ++ " = " ++ v) $ reverse cs) ++ reverse ls
