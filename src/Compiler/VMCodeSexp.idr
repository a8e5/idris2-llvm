module Compiler.VMCodeSexp

import Data.Either
import Data.List
import Data.Maybe
import Data.String
import Data.Vect

import Compiler.VMCode
import Core.CompileExpr
import Core.TT

import Data.Sexp

%default covering

ToSexp (Maybe Int) where
  toSexp (Just i) = SList [SAtom "Just", SAtom $ show i]
  toSexp (Nothing) = SList [SAtom "Nothing"]

FromSexp (Maybe Int) where
  fromSexp (SList [SAtom "Just", SAtom s]) = maybeToEither "invalid maybe int" $ map Just $ parseInteger s
  fromSexp (SList [SAtom "Nothing"]) = Right Nothing
  fromSexp s = Left $ "invalid maybe int: " ++ show s

export
total
ToSexp Name where
  toSexp (UN s) = SList [SAtom "UN", SAtom s]
  toSexp (NS ns n) = SList [SAtom "NS", SList (map SAtom (unsafeUnfoldNamespace ns)), toSexp n]
  toSexp (DN d n) = SList [SAtom "DN", SAtom d, toSexp n]
  toSexp (MN s i) = SList [SAtom "MN", SAtom s, SAtom $ cast i]
  toSexp (PV n i) = SList [SAtom "PV", toSexp n, SAtom $ cast i]
  toSexp (RF s) = SList [SAtom "RF", SAtom s]
  toSexp (CaseBlock outer i) = SList [SAtom "CaseBlock", SAtom $ outer, SAtom $ cast i]
  toSexp (WithBlock outer i) = SList [SAtom "WithBlock", SAtom $ outer, SAtom $ cast i]
  toSexp (Nested (outer, idx) inner) = SList [SAtom "Nested", SAtom $ cast outer, SAtom $ cast idx, toSexp inner]
  toSexp (Resolved i) = SList [SAtom "Resolved", SAtom $ cast i]

export
total
FromSexp Name where
  fromSexp (SList [SAtom "UN", SAtom s]) = Right $ (UN s)
  fromSexp (SList [SAtom "RF", SAtom s]) = Right $ (RF s)
  fromSexp (SList [SAtom "DN", SAtom d, n]) = pure $ (DN d !(fromSexp n))
  fromSexp (SList [SAtom "MN", SAtom s, SAtom i]) = pure $ (MN s !(maybeToEither "invalid MN int" $ parseInteger i))
  fromSexp (SList [SAtom "PV", n, SAtom i]) = pure $ (PV !(fromSexp n) !(maybeToEither "invalid PV int" $ parseInteger i))
  fromSexp (SList [SAtom "CaseBlock", SAtom o, SAtom i]) = pure $ (CaseBlock o !(maybeToEither "invalid caseblock inner int" $ parseInteger i))
  fromSexp (SList [SAtom "WithBlock", SAtom o, SAtom i]) = pure $ (WithBlock o !(maybeToEither "invalid withblock inner int" $ parseInteger i))
  fromSexp (SList [SAtom "NS", SList ns, n]) = do
    comps <- traverse (unAtom "namespace component") ns
    pure (NS (unsafeFoldNamespace comps) !(fromSexp n))
  fromSexp (SList [SAtom "Nested", SAtom o, SAtom i, inner]) = do
    outer <- maybeToEither "invalid nested outer int" $ parseInteger o
    idx <- maybeToEither "invalid nested idx int" $ parseInteger i
    n <- fromSexp inner
    pure (Nested (outer, idx) n)

  fromSexp n = Left $ ("error parsing name: " ++ show n)

ToSexp (Either Int Name) where
  toSexp (Left i) = SAtom $ show i
  toSexp (Right n) = toSexp n

FromSexp (Either Int Name) where
  fromSexp (SAtom i) = do
    tag <- maybeToEither "invalid int/name" (parseInteger i)
    pure (Left tag)
  fromSexp n = do
    pure $ Right !(fromSexp n)

ToSexp Reg where
  toSexp RVal = SAtom "RVAL"
  toSexp (Loc i) = SAtom ("v" ++ show i)
  toSexp Discard = SAtom "DISCARD"

shelper : ToSexp a => String -> List a -> Sexp
shelper s xs = SList ([SAtom s] ++ map toSexp xs)

export
ToSexp Constant where
  toSexp (I i)       = SList [SAtom "I", SAtom $ show i]
  toSexp (BI i)      = SList [SAtom "BI", SAtom $ show i]
  toSexp (Str s)     = SList [SAtom "Str", SAtom s]
  toSexp (Ch c)      = SList [SAtom "Ch", SAtom $ cast c]
  toSexp (Db d)      = SList [SAtom "Db", SAtom $ show d]
  toSexp WorldVal    = SList [SAtom "%World"]
  toSexp CharType    = SList [SAtom "Char"]
  toSexp IntType     = SList [SAtom "Int"]
  toSexp IntegerType = SList [SAtom "Integer"]
  toSexp StringType  = SList [SAtom "String"]
  toSexp DoubleType  = SList [SAtom "Double"]
  toSexp u           = SList [SAtom "const-not-implemented", SAtom $ show u]

export
total
FromSexp Constant where
  fromSexp (SList [SAtom "I", SAtom i]) = Right $ I $ cast i
  fromSexp (SList [SAtom "BI", SAtom i]) = Right $ BI $ cast i
  fromSexp (SList [SAtom "Str", SAtom s]) = Right $ Str s
  fromSexp (SList [SAtom "Ch", SAtom c]) = Right $ Ch $ assert_total $ strIndex c 0
  fromSexp (SList [SAtom "Db", SAtom d]) = Right $ Db $ cast d
  fromSexp (SList [SAtom "%World"]) = Right $ WorldVal
  fromSexp (SList [SAtom "Integer"]) = Right $ IntegerType
  fromSexp (SList [SAtom "Int"]) = Right $ IntType
  fromSexp (SList [SAtom "Char"]) = Right $ CharType
  fromSexp (SList [SAtom "String"]) = Right $ StringType
  fromSexp (SList [SAtom "Double"]) = Right $ DoubleType
  fromSexp s = Left $ "invalid constant: " ++ show s

export
total
ToSexp (PrimFn arity) where
  toSexp (Add ty)      = SList [SAtom "Add", toSexp ty]
  toSexp (Sub ty)      = SList [SAtom "Sub", toSexp ty]
  toSexp (Mul ty)      = SList [SAtom "Mul", toSexp ty]
  toSexp (Div ty)      = SList [SAtom "Div", toSexp ty]
  toSexp (Mod ty)      = SList [SAtom "Mod", toSexp ty]
  toSexp (Neg ty)      = SList [SAtom "Neg", toSexp ty]
  toSexp (ShiftL ty)   = SList [SAtom "ShiftL", toSexp ty]
  toSexp (ShiftR ty)   = SList [SAtom "ShiftR", toSexp ty]

  toSexp (BAnd ty)     = SList [SAtom "BAnd", toSexp ty]
  toSexp (BOr ty)      = SList [SAtom "BOr", toSexp ty]
  toSexp (BXOr ty)     = SList [SAtom "BXOr", toSexp ty]

  toSexp (LT ty)       = SList [SAtom "LT", toSexp ty]
  toSexp (LTE ty)      = SList [SAtom "LTE", toSexp ty]
  toSexp (EQ ty)       = SList [SAtom "EQ", toSexp ty]
  toSexp (GTE ty)      = SList [SAtom "GTE", toSexp ty]
  toSexp (GT ty)       = SList [SAtom "GT", toSexp ty]

  toSexp StrLength     = SList [SAtom "StrLength"]
  toSexp StrHead       = SList [SAtom "StrHead"]
  toSexp StrTail       = SList [SAtom "StrTail"]
  toSexp StrReverse    = SList [SAtom "StrReverse"]
  toSexp StrIndex      = SList [SAtom "StrIndex"]
  toSexp StrCons       = SList [SAtom "StrCons"]
  toSexp StrAppend     = SList [SAtom "StrAppend"]
  toSexp StrSubstr     = SList [SAtom "StrSubstr"]

  toSexp DoubleExp     = SList [SAtom "DoubleExp"]
  toSexp DoubleLog     = SList [SAtom "DoubleLog"]
  toSexp DoublePow     = SList [SAtom "DoublePow"]
  toSexp DoubleSin     = SList [SAtom "DoubleSin"]
  toSexp DoubleCos     = SList [SAtom "DoubleCos"]
  toSexp DoubleTan     = SList [SAtom "DoubleTan"]
  toSexp DoubleASin    = SList [SAtom "DoubleASin"]
  toSexp DoubleACos    = SList [SAtom "DoubleACos"]
  toSexp DoubleATan    = SList [SAtom "DoubleATan"]
  toSexp DoubleSqrt    = SList [SAtom "DoubleSqrt"]
  toSexp DoubleFloor   = SList [SAtom "DoubleFloor"]
  toSexp DoubleCeiling = SList [SAtom "DoubleCeiling"]

  toSexp (Cast t1 t2)  = SList [SAtom "Cast", toSexp t1, toSexp t2]
  toSexp BelieveMe     = SList [SAtom "BelieveMe"]
  toSexp Crash         = SList [SAtom "Crash"]

export
FromSexp (PrimFn 1) where
  fromSexp (SList [SAtom "Cast", t1, t2])  = pure $ Cast !(fromSexp t1) !(fromSexp t2)
  fromSexp (SList [SAtom "Neg", t])        = pure $ Neg !(fromSexp t)
  fromSexp (SList [SAtom "StrLength"])     = pure $ StrLength
  fromSexp (SList [SAtom "StrHead"])       = pure $ StrHead
  fromSexp (SList [SAtom "StrTail"])       = pure $ StrTail
  fromSexp (SList [SAtom "StrReverse"])    = pure $ StrReverse

  fromSexp (SList [SAtom "DoubleExp"])     = pure $ DoubleExp
  fromSexp (SList [SAtom "DoubleLog"])     = pure $ DoubleLog
  fromSexp (SList [SAtom "DoubleSin"])     = pure $ DoubleSin
  fromSexp (SList [SAtom "DoubleCos"])     = pure $ DoubleCos
  fromSexp (SList [SAtom "DoubleTan"])     = pure $ DoubleTan
  fromSexp (SList [SAtom "DoubleASin"])    = pure $ DoubleASin
  fromSexp (SList [SAtom "DoubleACos"])    = pure $ DoubleACos
  fromSexp (SList [SAtom "DoubleATan"])    = pure $ DoubleATan
  fromSexp (SList [SAtom "DoubleSqrt"])    = pure $ DoubleSqrt
  fromSexp (SList [SAtom "DoubleFloor"])   = pure $ DoubleFloor
  fromSexp (SList [SAtom "DoubleCeiling"]) = pure $ DoubleCeiling

  fromSexp s                               = Left $ "invalid PrimFn 1: " ++ show s

export
FromSexp (PrimFn 2) where
  fromSexp (SList [SAtom "Add", ty])    = Add <$> fromSexp ty
  fromSexp (SList [SAtom "Sub", ty])    = Sub <$> fromSexp ty
  fromSexp (SList [SAtom "Mul", ty])    = Mul <$> fromSexp ty
  fromSexp (SList [SAtom "Div", ty])    = Div <$> fromSexp ty
  fromSexp (SList [SAtom "Mod", ty])    = Mod <$> fromSexp ty
  fromSexp (SList [SAtom "ShiftL", ty]) = ShiftL <$> fromSexp ty
  fromSexp (SList [SAtom "ShiftR", ty]) = ShiftR <$> fromSexp ty

  fromSexp (SList [SAtom "BAnd", ty])   = BAnd <$> fromSexp ty
  fromSexp (SList [SAtom "BOr", ty])    = BOr <$> fromSexp ty
  fromSexp (SList [SAtom "BXOr", ty])   = BXOr <$> fromSexp ty

  fromSexp (SList [SAtom "LT", ty])     = LT <$> fromSexp ty
  fromSexp (SList [SAtom "LTE", ty])    = LTE <$> fromSexp ty
  fromSexp (SList [SAtom "EQ", ty])     = EQ <$> fromSexp ty
  fromSexp (SList [SAtom "GTE", ty])    = GTE <$> fromSexp ty
  fromSexp (SList [SAtom "GT", ty])     = GT <$> fromSexp ty

  fromSexp (SList [SAtom "StrIndex"])   = pure $ StrIndex
  fromSexp (SList [SAtom "StrCons"])    = pure $ StrCons
  fromSexp (SList [SAtom "StrAppend"])  = pure $ StrAppend

  fromSexp (SList [SAtom "DoublePow"])     = pure $ DoublePow

  fromSexp (SList [SAtom "Crash"])      = pure $ Crash

  fromSexp s = Left $ "invalid PrimFn 2: " ++ show s

export
FromSexp (PrimFn 3) where
  fromSexp (SList [SAtom "StrSubstr"])  = pure $ StrSubstr
  fromSexp (SList [SAtom "BelieveMe"]) = pure $ BelieveMe
  fromSexp s = Left $ "invalid PrimFn 3: " ++ show s

public export
ToSexp VMInst where
  toSexp (DECLARE r) = SList [SAtom "DECLARE", toSexp r]
  toSexp START = SList [SAtom "START"]
  toSexp (ASSIGN d s) = shelper "ASSIGN" [d, s]
  toSexp (MKCON reg tag args) = SList $ [SAtom "MKCON", toSexp reg, toSexp tag, SList $ map toSexp args]
  toSexp (MKCLOSURE reg n missing args) = SList $ [SAtom "MKCLOSURE", toSexp reg, toSexp n, SAtom $ show missing, SList (map toSexp args)]
  toSexp (MKCONSTANT reg const) = SList $ [SAtom "MKCONSTANT", toSexp reg, toSexp const]
  toSexp (CALL reg isTail n args) = SList $ [SAtom "CALL", toSexp reg, SAtom $ show isTail, toSexp n, SList $ map toSexp args]
  toSexp (OP reg op args) = SList $ [SAtom "OP", toSexp reg, toSexp op] ++ map toSexp (toList args)
  toSexp (APPLY reg f a) = shelper "APPLY" [reg, f, a]
  toSexp (CASE reg alts def) = SList ([SAtom "CASE", toSexp reg, defaultCase def] ++ (map altToSexp alts)) where
    defaultCase : Maybe (List VMInst) -> Sexp
    defaultCase def = case def of
                           Nothing => SList [SAtom "nodefault"]
                           Just insts => SList [SAtom "default", SList $ assert_total $ map toSexp insts]

    altToSexp : (Either Int Name, List VMInst) -> Sexp
    altToSexp (c, insts) = SList [toSexp c, SList $ assert_total $ map toSexp insts]
  toSexp (CONSTCASE reg alts def) = SList ([SAtom "CONSTCASE", toSexp reg, defaultCase def] ++ (map altToSexp alts)) where
    defaultCase : Maybe (List VMInst) -> Sexp
    defaultCase def = case def of
                           Nothing => SList [SAtom "nodefault"]
                           Just insts => SList [SAtom "default", SList $ assert_total $ map toSexp insts]

    altToSexp : (Constant, List VMInst) -> Sexp
    altToSexp (c, insts) = SList [toSexp c, SList $ assert_total $ map toSexp insts]
  toSexp (PROJECT reg val pos) = SList [SAtom "PROJECT", toSexp reg, toSexp val, SAtom $ show pos]
  toSexp (EXTPRIM r name args) = SList [SAtom "EXTPRIM", toSexp r, toSexp name, SList (map toSexp args)]
  toSexp (NULL r) = SList [SAtom "NULL", toSexp r]
  toSexp (ERROR msg) = SList [SAtom "ERROR", SAtom msg]

export
FromSexp Bool where
  fromSexp (SAtom "True") = Right $ True
  fromSexp (SAtom "False") = Right $ False
  fromSexp _ = Left $ "invalid Bool"

FromSexp Reg where
  fromSexp (SAtom "RVAL") = Right RVal
  fromSexp (SAtom "DISCARD") = Right Discard
  fromSexp (SAtom "") = Left "invalid reg: \"\""
  fromSexp (SAtom s) = Right (Loc $ cast $ assert_total $ strTail s)
  fromSexp s = Left ("invalid reg: " ++ show s)

collectFromSexp : FromSexp a => List Sexp -> Either String (List a)
collectFromSexp s = traverse fromSexp s

export
FromSexp VMInst where
  fromSexp (SList [SAtom "DECLARE", r]) = fromSexp r >>= pure . DECLARE
  fromSexp (SList [SAtom "START"]) = Right START
  fromSexp (SList [SAtom "ASSIGN", d, s]) = do
    pd <- fromSexp d
    ps <- fromSexp s
    pure $ ASSIGN pd ps
  fromSexp (SList [SAtom "MKCON", regS, tagS, SList argsS]) = do
    reg <- fromSexp regS
    tag <- fromSexp tagS
    args <- collectFromSexp argsS
    pure $ MKCON reg tag args
  fromSexp (SList [SAtom "MKCLOSURE", regS, nameS, SAtom missingStr, SList argsS]) = do
    reg <- fromSexp regS
    name <- fromSexp nameS
    args <- collectFromSexp argsS
    pure $ MKCLOSURE reg name (stringToNatOrZ missingStr) args
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
  fromSexp (SList ((SAtom "OP")::regS::nameS::argsS)) = do
    reg <- fromSexp regS
    args <- collectFromSexp argsS
    case (nameS, args) of
         (op, [a]) => pure $ OP reg !(fromSexp op) [a]
         (op, [a, b]) => pure $ OP reg !(fromSexp op) [a, b]
         (op, [a, b, c]) => pure $ OP reg !(fromSexp op) [a, b, c]
         (op, args) => Left $ "invalid op: " ++ show (op, args)
    --pure $ OP reg name args
  fromSexp (SList [SAtom "APPLY", regS, fS, argS]) = do
    reg <- fromSexp regS
    f <- fromSexp fS
    arg <- fromSexp argS
    pure $ APPLY reg f arg
  fromSexp (SList ((SAtom "CONSTCASE")::regS::defaultS::altsS)) =
    do reg <- fromSexp regS
       defaultV <- assert_total $ readDefault defaultS
       pure $ CONSTCASE reg !(traverse readAlt altsS) defaultV
    where
        readDefault : Sexp -> Either String (Maybe (List VMInst))
        readDefault (SList [SAtom "nodefault"]) = Right Nothing
        readDefault (SList [SAtom "default", SList is]) = do
          insts <- collectFromSexp is
          pure $ Just insts
        readDefault _ = Right Nothing --Left "invalid default"
        readAlt : Sexp -> Either String (Constant, (List VMInst))
        readAlt (SList [c, SList is]) = do
          constant <- fromSexp c
          insts <- collectFromSexp is
          pure $ (constant, insts)
        readAlt _ = Left $ "error in alt"
  fromSexp (SList ((SAtom "CASE")::regS::defaultS::altsS)) =
    (do
      reg <- fromSexp regS
      defaultV <- assert_total $ readDefault defaultS
      pure $ CASE reg !(traverse readAlt altsS) defaultV
      )
      where
        readDefault : Sexp -> Either String (Maybe (List VMInst))
        readDefault (SList [SAtom "nodefault"]) = Right Nothing
        readDefault (SList [SAtom "default", SList is]) = do
          insts <- collectFromSexp is
          pure $ Just insts
        readDefault _ = Right Nothing --Left "invalid default"
        readAlt : Sexp -> Either String (Either Int Name, (List VMInst))
        readAlt (SList [tagOrNameS, SList is]) = do
          tagOrName <- fromSexp tagOrNameS
          insts <- collectFromSexp is
          pure $ (tagOrName, insts)
        readAlt _ = Left $ "error in alt"
  fromSexp (SList ((SAtom "EXTPRIM")::regS::nameS::(SList argsS)::[])) = do
    reg <- fromSexp regS
    args <- collectFromSexp argsS
    pure $ EXTPRIM reg !(fromSexp nameS) args
  fromSexp (SList ((SAtom "PROJECT")::regS::objS::(SAtom posS)::[])) = do
    reg <- fromSexp regS
    obj <- fromSexp objS
    pos <- maybeToEither ("invalid int in PROJECT pos: " ++ posS) $ parseInteger posS
    pure $ PROJECT reg obj pos
  fromSexp (SList ((SAtom "ERROR")::(SAtom msg)::[])) = do
    pure $ ERROR msg
  fromSexp sexp = Left $ "vminst not impl" ++ show sexp

export
ToSexp CFType where
  toSexp t = SList [SAtom $ show t]

public export
ToSexp (Name, VMDef) where
  toSexp (n, (MkVMFun args insts)) = SList $ [SAtom "defun", toSexp n, SList $ map (\i => SAtom $ "v" ++ show i) args, SList $ map toSexp insts]
  toSexp (n, (MkVMError insts)) = SList $ [SAtom "deferr", toSexp n, SList $ map toSexp insts]
  toSexp (n, (MkVMForeign cs args ret)) = SList $ [SAtom "foreign", toSexp n, SList $ map SAtom cs, SList $ map toSexp args, toSexp ret]

FromSexp CFType where
  fromSexp (SList [SAtom "Buffer"]) = pure CFBuffer
  fromSexp (SList [SAtom "Double"]) = pure CFDouble
  fromSexp (SList [SAtom "Int"]) = pure CFInt
  fromSexp (SList [SAtom "IORes Buffer"]) = pure $ CFIORes CFBuffer
  fromSexp (SList [SAtom "IORes Double"]) = pure $ CFIORes CFDouble
  fromSexp (SList [SAtom "IORes Int"]) = pure $ CFIORes CFInt
  fromSexp (SList [SAtom "IORes String"]) = pure $ CFIORes CFString
  fromSexp (SList [SAtom "IORes Unit"]) = pure $ CFIORes CFUnit
  fromSexp (SList [SAtom "IORes Ptr"]) = pure $ CFIORes CFPtr
  fromSexp (SList [SAtom "IORes Prelude.List String"]) = pure $ CFIORes (CFUser (NS (unsafeFoldNamespace ["Prelude"]) (UN "List")) [CFString])
  fromSexp (SList [SAtom "Ptr"]) = pure CFPtr
  fromSexp (SList [SAtom "String"]) = pure CFString
  fromSexp (SList [SAtom "%World"]) = pure CFWorld
  fromSexp s = Left $ "invalid CFType: " ++ show s

getArg : Sexp -> Either String Int
getArg (SAtom s) = maybeToEither "invalid int" $ parseInteger $ assert_total $ strTail s
getArg x = Left "invalid ARG"

export
FromSexp (Name, VMDef) where
  fromSexp (SList [SAtom "defun", n, SList args, SList insts]) = do
    name <- fromSexp n
    fArgs <- traverse getArg args
    fInsts <- collectFromSexp insts
    pure (name, MkVMFun fArgs fInsts)
  fromSexp (SList [SAtom "foreign", n, SList extS, SList argsS, retS]) = do
    name <- fromSexp n
    cs <- traverse (unAtom "foreign name") extS
    args <- traverse fromSexp argsS
    ret <- fromSexp retS
    pure (name, MkVMForeign cs args ret)
  fromSexp l = Left ("invalid vmdef: " ++ show l)

export
getVMDefs : List Sexp -> Either String (List (Name, VMDef))
getVMDefs s = either (\error=>Left ("failed to read VMCode from Sexp: " ++ error ++ "\n")) Right $ traverse fromSexp s
