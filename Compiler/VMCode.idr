module Compiler.VMCode

import Data.Sexp

%default covering

showSep : String -> List String -> String
showSep s xs = go xs where
  go' : List String -> String
  go' xs = concat $ map ((++) s) xs

  go : List String -> String
  go [] = ""
  go [x] = x
  go (x::rest) = x ++ go' rest

public export
data Name = MkName String

public export
ToSexp String where
  toSexp n = SAtom ("\"" ++ (show n) ++ "\"")

export
ToSexp Name where
  toSexp (MkName n) = SAtom ("\"" ++ (show n) ++ "\"")

export
Show Name where
  show (MkName s) = s

export
FromSexp Name where
  fromSexp (SAtom s) = Right $ MkName s
  fromSexp _ = Left $ "not a name"

export
FromSexp Bool where
  fromSexp (SAtom "True") = Right $ True
  fromSexp (SAtom "False") = Right $ False
  fromSexp _ = Left $ "invalid Bool"

public export
data Reg : Type where
     RVal : Reg
     Loc : Int -> Reg
     Discard : Reg

ToSexp Reg where
  toSexp RVal = SAtom "RVAL"
  toSexp (Loc i) = SAtom ("v" ++ show i)
  toSexp Discard = SAtom "DISCARD"

FromSexp Reg where
  fromSexp (SAtom "RVAL") = Right RVal
  fromSexp (SAtom "DISCARD") = Right Discard
  fromSexp (SAtom "") = Left "invalid reg: \"\""
  fromSexp (SAtom s) = Right (Loc $ cast $ assert_total $ strTail s)
  fromSexp s = Left ("invalid reg: " ++ show s)

public export
data Constant : Type where
  MkConstant : String -> Constant

Show Constant where
  show (MkConstant s) = show s

export
FromSexp Constant where
  fromSexp (SAtom s) = Right $ MkConstant s
  fromSexp _ = Left "invalid constant"

-- VM instructions - first Reg is where the result goes, unless stated
-- otherwise.

-- As long as you have a representation of closures, and an 'apply' function
-- which adds an argument and evaluates if it's fully applied, then you can
-- translate this directly to a target language program.
public export
data VMInst : Type where
     DECLARE : Reg -> VMInst
     START : VMInst -- start of the main body of the function
     ASSIGN : Reg -> Reg -> VMInst
     MKCON : Reg -> (tag : Int) -> (args : List Reg) -> VMInst
     MKCLOSURE : Reg -> Name -> (missing : Nat) -> (args : List Reg) -> VMInst
     MKCONSTANT : Reg -> Constant -> VMInst
     APPLY : Reg -> (f : Reg) -> (a : Reg) -> VMInst
     CALL : Reg -> (tailpos : Bool) -> Name -> (args : List Reg) -> VMInst
     OP : Reg -> String -> List Reg -> VMInst
     EXTPRIM : Reg -> String -> List Reg -> VMInst

     CASE : Reg -> -- scrutinee 
            (alts : List (Int, List VMInst)) -> -- based on constructor tag
            (def : Maybe (List VMInst)) ->
            VMInst
     CONSTCASE : Reg -> -- scrutinee 
                 (alts : List (Constant, List VMInst)) ->
                 (def : Maybe (List VMInst)) ->
                 VMInst
     PROJECT : Reg -> (value : Reg) -> (pos : Int) -> VMInst
     NULL : Reg -> VMInst

     ERROR : String -> VMInst

shelper : ToSexp a => String -> List a -> Sexp
shelper s xs = SList ([SAtom s] ++ map toSexp xs)

public export
data VMDef : Type where
     MkVMFun : (args : List Reg) -> List VMInst -> VMDef
     MkVMError : List VMInst -> VMDef

export
Show Reg where
  show RVal = "RVAL"
  show (Loc i) = "v" ++ show i
  show Discard = "DISCARD"

export
Show VMInst where
  show (DECLARE r) = "DECLARE " ++ show r
  show START = "START"
  show (ASSIGN r v) = show r ++ " := " ++ show v
  show (MKCON r t args)
      = show r ++ " := MKCON " ++ show t ++ " (" ++
                  showSep ", " (map show args) ++ ")"
  show (MKCLOSURE r n m args)
      = show r ++ " := MKCLOSURE " ++ show n ++ " " ++ show m ++ " (" ++
                  showSep ", " (map show args) ++ ")"
  show (MKCONSTANT r c) = show r ++ " := MKCONSTANT " ++ show c
  show (APPLY r f a) = show r ++ " := " ++ show f ++ " @ " ++ show a
  show (CALL r t n args)
      = show r ++ " := " ++ (if t then "TAILCALL " else "CALL ") ++
        show n ++ "(" ++ showSep ", " (map show args) ++ ")"
  show (OP r op args)
      = show r ++ " := " ++ "OP " ++
        show op ++ "(" ++ showSep ", " (map show (toList args)) ++ ")"
  show (EXTPRIM r n args)
      = show r ++ " := " ++ "EXTPRIM " ++
        show n ++ "(" ++ showSep ", " (map show args) ++ ")"

  show (CASE scr alts def)
      = "CASE " ++ show scr ++ " " ++ show alts ++ " {default: " ++ show def ++ "}"
  show (CONSTCASE scr alts def)
      = "CASE " ++ show scr ++ " " ++ show alts ++ " {default: " ++ show def ++ "}"

  show (PROJECT r val pos)
      = show r ++ " := PROJECT(" ++ show val ++ ", " ++ show pos ++ ")"
  show (NULL r) = show r ++ " := NULL"
  show (ERROR str) = "ERROR " ++ show str

public export
ToSexp Constant where
  toSexp c = SAtom $ show c

public export
ToSexp VMInst where
  toSexp (DECLARE r) = SList [SAtom "DECLARE", toSexp r]
  toSexp START = SList [SAtom "START"]
  toSexp (ASSIGN d s) = shelper "ASSIGN" [d, s]
  toSexp (MKCON reg tag args) = SList $ [SAtom "MKCON", toSexp reg, SAtom $ show tag, SList $ map toSexp args]
  toSexp (MKCLOSURE reg n missing args) = SList $ [SAtom "MKCLOSURE", toSexp reg, toSexp n, SAtom $ show missing, SList (map toSexp args)]
  toSexp (MKCONSTANT reg const) = SList $ [SAtom "MKCONSTANT", toSexp reg, toSexp const]
  toSexp (CALL reg isTail n args) = SList $ [SAtom "CALL", toSexp reg, SAtom $ show isTail, toSexp n, SList $ map toSexp args]
  toSexp (OP reg op args) = SList $ [SAtom "OP", toSexp reg, SAtom $ show op] ++ map toSexp (toList args)
  toSexp (APPLY reg f a) = shelper "APPLY" [reg, f, a]
  toSexp (CASE reg alts def) = SList ([SAtom "CASE", toSexp reg, defaultCase def] ++ (map altToSexp alts)) where
    defaultCase : Maybe (List VMInst) -> Sexp
    defaultCase def = case def of
                           Nothing => SList [SAtom "nodefault"]
                           Just insts => SList [SAtom "default", SList $ assert_total $ map toSexp insts]

    altToSexp : (Int, List VMInst) -> Sexp
    altToSexp (i, insts) = SList [SAtom $ show i, SList $ assert_total $ map toSexp insts]
  toSexp (CONSTCASE reg alts def) = SList ([SAtom "CONSTCASE", toSexp reg, defaultCase def] ++ (map altToSexp alts)) where
    defaultCase : Maybe (List VMInst) -> Sexp
    defaultCase def = case def of
                           Nothing => SList [SAtom "nodefault"]
                           Just insts => SList [SAtom "default", SList $ assert_total $ map toSexp insts]

    altToSexp : (Constant, List VMInst) -> Sexp
    altToSexp (c, insts) = SList [toSexp c, SList $ assert_total $ map toSexp insts]
  toSexp (PROJECT reg val pos) = SList [toSexp reg, toSexp val, SAtom $ show pos]
  toSexp u = SList [SAtom "not-implemented", SAtom ("\"" ++ (assert_total $ show u) ++ "\"")]
  {-toSexp ASSIGN = SList -}

collectFromSexp : FromSexp a => List Sexp -> Either String (List a)
collectFromSexp s = Right $ rights $ map fromSexp s

export
FromSexp VMInst where
  fromSexp (SList [SAtom "DECLARE", r]) = fromSexp r >>= pure . DECLARE
  fromSexp (SList [SAtom "START"]) = Right START
  fromSexp (SList [SAtom "ASSIGN", d, s]) = do
    pd <- fromSexp d
    ps <- fromSexp s
    pure $ ASSIGN pd ps
  fromSexp (SList [SAtom "MKCON", regS, SAtom tagS, SList argsS]) = do
    reg <- fromSexp regS
    let tag = cast tagS
    args <- collectFromSexp argsS
    pure $ MKCON reg tag args
  fromSexp (SList [SAtom "MKCLOSURE", regS, nameS, SAtom missingStr, SList argsS]) = do
    reg <- fromSexp regS
    name <- fromSexp nameS
    args <- collectFromSexp argsS
    pure $ MKCLOSURE reg name (cast missingStr) args
  fromSexp (SList [SAtom "MKCONSTANT", regS, constS]) = do
    reg <- fromSexp regS
    const <- fromSexp constS
    pure $ MKCONSTANT reg const
  fromSexp (SList [SAtom "CALL", regS, tailS, nameS, SList argsS]) = do
    reg <- fromSexp regS
    name <- fromSexp nameS
    tail <- fromSexp tailS
    args <- collectFromSexp argsS
    pure $ CALL reg tail name args
  fromSexp (SList ((SAtom "OP")::regS::(SAtom name)::argsS)) = do
    reg <- fromSexp regS
    args <- collectFromSexp argsS
    pure $ OP reg name args
  fromSexp (SList [SAtom "APPLY", regS, fS, argS]) = do
    reg <- fromSexp regS
    f <- fromSexp fS
    arg <- fromSexp argS
    pure $ APPLY reg f arg
  fromSexp (SList ((SAtom "CASE")::regS::defaultS::altsS)) =
    (do
      reg <- fromSexp regS
      default <- assert_total $ readDefault defaultS
      pure $ CASE reg [] default
      )
      where
        readDefault : Sexp -> Either String (Maybe (List VMInst))
        readDefault (SList [SAtom "nodefault"]) = Right Nothing
        readDefault (SList [SAtom "default", SList is]) = do
          insts <- collectFromSexp is
          pure $ Just insts
        readDefault _ = Right Nothing --Left "invalid default"
  fromSexp _ = Left "vminst not impl"

public export
ToSexp VMDef where
  toSexp (MkVMFun args insts) = SList $ [SAtom "fun", SList $ map (\i => SAtom $ "v" ++ show i) args, SList $ map toSexp insts]
  toSexp (MkVMError insts) = SList $ [SAtom "error", SList $ map toSexp insts]

public export
ToSexp (String, VMDef) where
  toSexp (n, (MkVMFun args insts)) = SList $ [SAtom "defun", toSexp n, SList $ map (\i => SAtom $ "v" ++ show i) args, SList $ map toSexp insts]
  toSexp (n, (MkVMError insts)) = SList $ [SAtom "deferr", toSexp n, SList $ map toSexp insts]

{-listFromSexp : {FromSexp a} => FromSexp (List a)-}
{-listFromSexp {fa} =-}
  {-let mapped = map fromSexp s-}

{-export-}
{-FromSexp a => FromSexp (List a) where-}
  {-fromSexp (SAtom _) = Left "not a list"-}
  {-fromSexp s = let mapped = map fromSexp s in-}
                   {-?fmmm-}

export
FromSexp (String, VMDef) where
  fromSexp (SList [SAtom "defun", SAtom n, SList args, SList insts]) = do
    fArgs <- collectFromSexp args
    fInsts <- collectFromSexp insts
    pure (n, MkVMFun fArgs fInsts)
  fromSexp _ = Left "invalid vmdef"

export
Show VMDef where
  show (MkVMFun args body) = show args ++ ": \n  " ++ (showSep "\n  " (map show body)) ++ "\n"
  show (MkVMError err) = "Error: " ++ show err

