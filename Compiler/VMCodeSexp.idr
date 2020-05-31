module Compiler.VMCodeSexp

import Data.Either
import Data.List
import Data.Maybe
import Data.Strings
import Data.Vect

import Codegen
import Compiler.VMCode
import Core.TT
import Data.Sexp
import Utils.Hex

ToSexp (Maybe Int) where
  toSexp (Just i) = SList [SAtom "Just", SAtom $ show i]
  toSexp (Nothing) = SList [SAtom "Nothing"]

FromSexp (Maybe Int) where
  fromSexp (SList [SAtom "Just", SAtom s]) = maybeToEither "invalid maybe int" $ map Just $ parseInteger s
  fromSexp (SList [SAtom "Nothing"]) = Right Nothing
  fromSexp s = Left $ "invalid maybe int: " ++ show s

export
ToSexp Name where
  toSexp (UN s) = SList [SAtom "UN", SAtom s]
  toSexp (NS ns n) = SList [SAtom "NS", SList (map SAtom ns), toSexp n]
  toSexp (DN d n) = SList [SAtom "DN", SAtom d, toSexp n]
  toSexp (MN s i) = SList [SAtom "MN", SAtom s, SAtom $ cast i]
  toSexp (CaseBlock outer i) = SList [SAtom "CaseBlock", SAtom $ cast outer, SAtom $ cast i]
  toSexp (Nested (outer, idx) inner) = SList [SAtom "Nested", SAtom $ cast outer, SAtom $ cast idx, toSexp inner]
  toSexp n = assert_total $ (idris_crash $ "error-name:" ++ show n)

export
FromSexp Name where
  fromSexp (SList [SAtom "UN", SAtom s]) = Right $ (UN s)
  fromSexp (SList [SAtom "DN", SAtom d, n]) = pure $ (DN d !(fromSexp n))
  fromSexp (SList [SAtom "MN", SAtom s, SAtom i]) = pure $ (MN s !(maybeToEither "invalid MN int" $ parseInteger i))
  fromSexp (SList [SAtom "CaseBlock", SAtom o, SAtom i]) = pure $ (CaseBlock !(maybeToEither "invalid caseblock outer int" $ parseInteger o) !(maybeToEither "invalid caseblock inner int" $ parseInteger i))
  fromSexp (SList [SAtom "NS", SList ns, n]) = do
    comps <- traverse (unAtom "namespace component") ns
    pure (NS comps !(fromSexp n))
  fromSexp (SList [SAtom "Nested", SAtom o, SAtom i, inner]) = do
    outer <- maybeToEither "invalid nested outer int" $ parseInteger o
    idx <- maybeToEither "invalid nested idx int" $ parseInteger i
    n <- fromSexp inner
    pure (Nested (outer, idx) n)

  fromSexp n = Left $ ("error parsing name: " ++ show n)

ToSexp Reg where
  toSexp RVal = SAtom "RVAL"
  toSexp (Loc i) = SAtom ("v" ++ show i)
  toSexp Discard = SAtom "DISCARD"

shelper : ToSexp a => String -> List a -> Sexp
shelper s xs = SList ([SAtom s] ++ map toSexp xs)

public export
ToSexp Constant where
  toSexp (I i)    = SList [SAtom "I", SAtom $ show i]
  toSexp (BI i)   = SList [SAtom "BI", SAtom $ show i]
  toSexp (Str s)  = SList [SAtom "Str", SAtom s]
  toSexp (Ch c)   = SList [SAtom "Ch", SAtom $ show c]
  toSexp (Db d)   = SList [SAtom "Db", SAtom $ show d]
  toSexp WorldVal = SList [SAtom "%World"]
  toSexp u        = SList [SAtom "not-implemented", SAtom $ show u]

public export
ToSexp VMInst where
  toSexp (DECLARE r) = SList [SAtom "DECLARE", toSexp r]
  toSexp START = SList [SAtom "START"]
  toSexp (ASSIGN d s) = shelper "ASSIGN" [d, s]
  toSexp (MKCON reg tag args) = SList $ [SAtom "MKCON", toSexp reg, toSexp tag, SList $ map toSexp args]
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

    altToSexp : (Either Int Name, List VMInst) -> Sexp
    altToSexp (Left i, insts) = SList [SAtom $ show i, SList $ assert_total $ map toSexp insts]
    altToSexp (Right n, insts) = SList [toSexp n, SList $ assert_total $ map toSexp insts]
  toSexp (CONSTCASE reg alts def) = SList ([SAtom "CONSTCASE", toSexp reg, defaultCase def] ++ (map altToSexp alts)) where
    defaultCase : Maybe (List VMInst) -> Sexp
    defaultCase def = case def of
                           Nothing => SList [SAtom "nodefault"]
                           Just insts => SList [SAtom "default", SList $ assert_total $ map toSexp insts]

    altToSexp : (Constant, List VMInst) -> Sexp
    altToSexp (c, insts) = SList [toSexp c, SList $ assert_total $ map toSexp insts]
  toSexp (PROJECT reg val pos) = SList [SAtom "PROJECT", toSexp reg, toSexp val, SAtom $ show pos]
  toSexp u = SList [SAtom "not-implemented", SAtom ("\"" ++ (assert_total $ show u) ++ "\"")]
  {-toSexp ASSIGN = SList -}


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

export
FromSexp Constant where
  fromSexp (SList [SAtom "I", SAtom i]) = Right $ I $ cast i
  fromSexp (SList [SAtom "BI", SAtom i]) = Right $ BI $ cast i
  fromSexp (SList [SAtom "Str", SAtom s]) = Right $ Str s
  fromSexp (SList [SAtom "Ch", SAtom c]) = Right $ Ch $ assert_total $ strIndex c 1
  fromSexp (SList [SAtom "Db", SAtom d]) = Right $ Db $ cast d
  fromSexp (SList [SAtom "%World"]) = Right $ WorldVal
  fromSexp s = Left $ "invalid constant: " ++ show s

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
  fromSexp (SList ((SAtom "OP")::regS::(SAtom name)::argsS)) = do
    reg <- fromSexp regS
    args <- collectFromSexp argsS
    case (name, args) of
         ("+Int", [a,b]) => pure $ OP reg (Add IntType) [a,b]
         ("-Int", [a,b]) => pure $ OP reg (Sub IntType) [a,b]
         ("*Int", [a,b]) => pure $ OP reg (Mul IntType) [a,b]
         ("/Int", [a,b]) => pure $ OP reg (Div IntType) [a,b]
         ("%Int", [a,b]) => pure $ OP reg (Mod IntType) [a,b]
         ("<Int", [a,b]) => pure $ OP reg (LT IntType) [a,b]
         ("<=Int", [a,b]) => pure $ OP reg (LTE IntType) [a,b]
         (">Int", [a,b]) => pure $ OP reg (GT IntType) [a,b]
         (">=Int", [a,b]) => pure $ OP reg (GTE IntType) [a,b]
         ("==Int", [a,b]) => pure $ OP reg (EQ IntType) [a,b]
         ("+Integer", [a,b]) => pure $ OP reg (Add IntegerType) [a,b]
         ("-Integer", [a,b]) => pure $ OP reg (Sub IntegerType) [a,b]
         ("*Integer", [a,b]) => pure $ OP reg (Mul IntegerType) [a,b]
         ("==Integer", [a,b]) => pure $ OP reg (EQ IntegerType) [a,b]
         ("<=Integer", [a,b]) => pure $ OP reg (LTE IntegerType) [a,b]
         (">=Integer", [a,b]) => pure $ OP reg (GTE IntegerType) [a,b]
         (">Integer", [a,b]) => pure $ OP reg (GT IntegerType) [a,b]
         ("<Integer", [a,b]) => pure $ OP reg (LT IntegerType) [a,b]
         ("shl Integer", [a,b]) => pure $ OP reg (ShiftL IntegerType) [a,b]
         ("shr Integer", [a,b]) => pure $ OP reg (ShiftR IntegerType) [a,b]
         ("==Char", [a,b]) => pure $ OP reg (EQ CharType) [a,b]
         ("<=Char", [a,b]) => pure $ OP reg (LTE CharType) [a,b]
         ("<Char", [a,b]) => pure $ OP reg (LT CharType) [a,b]
         (">=Char", [a,b]) => pure $ OP reg (GTE CharType) [a,b]
         (">Char", [a,b]) => pure $ OP reg (GT CharType) [a,b]
         ("cast-String-Integer", [i]) => pure $ OP reg (Cast StringType IntegerType) [i]
         ("cast-String-Int", [i]) => pure $ OP reg (Cast StringType IntType) [i]
         ("cast-Integer-String", [i]) => pure $ OP reg (Cast IntegerType StringType) [i]
         ("cast-Int-String", [i]) => pure $ OP reg (Cast IntType StringType) [i]
         ("cast-Int-Char", [i]) => pure $ OP reg (Cast IntType CharType) [i]
         ("cast-Integer-Char", [i]) => pure $ OP reg (Cast IntegerType CharType) [i]
         ("cast-Int-Integer", [i]) => pure $ OP reg (Cast IntType IntegerType) [i]
         ("cast-Integer-Int", [i]) => pure $ OP reg (Cast IntegerType IntType) [i]
         ("cast-Char-Integer", [i]) => pure $ OP reg (Cast CharType IntegerType) [i]
         ("cast-Char-Int", [i]) => pure $ OP reg (Cast CharType IntType) [i]
         ("cast-Char-String", [i]) => pure $ OP reg (Cast CharType StringType) [i]
         ("cast-Double-String", [i]) => pure $ OP reg (Cast DoubleType StringType) [i]
         ("cast-String-Double", [i]) => pure $ OP reg (Cast StringType DoubleType) [i]
         ("==String", [a,b]) => pure $ OP reg (EQ StringType) [a,b]
         ("++", [a,b]) => pure $ OP reg (StrAppend) [a,b]
         ("op_strlen", [s]) => pure $ OP reg (StrLength) [s]
         ("op_strrev", [s]) => pure $ OP reg (StrReverse) [s]
         ("op_strhead", [s]) => pure $ OP reg (StrHead) [s]
         ("op_strtail", [s]) => pure $ OP reg (StrTail) [s]
         ("op_strcons", [a, b]) => pure $ OP reg (StrCons) [a,b]
         ("op_strindex", [a, b]) => pure $ OP reg (StrIndex) [a,b]
         ("believe_me", [a,b,c]) => pure $ OP reg (BelieveMe) [a,b,c]
         ("crash", [a,b]) => pure $ OP reg (Crash) [a,b]
         (op, _) => Left $ "invalid op: " ++ op
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
          tagOrName <- case tagOrNameS of
                            SAtom i => Right $ mirror $ maybeToEither (UN i) $ parseInteger i
                            x => mirror <$> Left <$> fromSexp x
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
  fromSexp sexp = Left $ "vminst not impl" ++ show sexp

public export
ToSexp VMDef where
  toSexp (MkVMFun args insts) = SList $ [SAtom "fun", SList $ map (\i => SAtom $ "v" ++ show i) args, SList $ map toSexp insts]
  toSexp (MkVMError insts) = SList $ [SAtom "error", SList $ map toSexp insts]

public export
ToSexp (Name, VMDef) where
  toSexp (n, (MkVMFun args insts)) = SList $ [SAtom "defun", toSexp n, SList $ map (\i => SAtom $ "v" ++ show i) args, SList $ map toSexp insts]
  toSexp (n, (MkVMError insts)) = SList $ [SAtom "deferr", toSexp n, SList $ map toSexp insts]

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
  fromSexp _ = Left "invalid vmdef"

export
partial
getVMDefs : List Sexp -> List (Name, VMDef)
getVMDefs s = either (\error=>idris_crash ("failed to read VMCode from Sexp: " ++ error ++ "\n" ++ show s)) id $ traverse fromSexp s
