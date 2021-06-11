module Control.Codegen

import public Control.Monad.State
import Data.List
import Data.String

import Debug.Trace

ConstDef : Type
ConstDef = (String, String)

export
data CGBuffer : Type where
  MkCGBuf : Int -> List ConstDef -> List String -> List String -> CGBuffer

public export
Codegen : Type -> Type
Codegen = State CGBuffer

emptyCG : CGBuffer
emptyCG = MkCGBuf 0 [] [] []

export
appendCode : String -> Codegen ()
appendCode c = modify $ \(MkCGBuf i consts l e) => (MkCGBuf i consts (c::l) e)

export
getUnique : Codegen Int
getUnique = do
  (MkCGBuf i c l e) <- get
  put (MkCGBuf (i+1) c l e)
  pure i

export
addConstant : Int -> String -> Codegen String
addConstant i v = do
  ci <- getUnique
  let name = "@glob_" ++ show i ++ "_c" ++ show ci
  (MkCGBuf i c l e) <- get
  put (MkCGBuf i ((name, v)::c) l e)
  pure name

export
addError : String -> Codegen ()
addError msg = do
  appendCode ("; ERROR: " ++ msg)
  (MkCGBuf i c l e) <- get
  put $ trace ("add error: " ++ msg) (MkCGBuf i c l (msg::e))

export
addMetadata : Int -> String -> Codegen String
addMetadata i v = do
  u <- getUnique
  let mdId = u * 0x10000 + i
  let name = "!" ++ show mdId
  (MkCGBuf i c l e) <- get
  put (MkCGBuf i ((name, v)::c) l e)
  pure name

export
runCodegen : Codegen () -> String
runCodegen r = let (MkCGBuf _ cs ls errors) = fst $ runState emptyCG r in
                    fastAppend $ intersperse "\n" $ (map (\(n,v) => n ++ " = " ++ v) $ reverse cs) ++ reverse ls
