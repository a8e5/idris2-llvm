module Compiler.LLVM.Rapid.Integer

import Data.Bits
import Data.Vect

import Compiler.LLVM.IR
import Compiler.LLVM.Instruction
import Compiler.LLVM.Rapid.Object
import Control.Codegen

GMP_LIMB_SIZE : Integer
GMP_LIMB_SIZE = 8

GMP_LIMB_BITS : Integer
GMP_LIMB_BITS = 8 * GMP_LIMB_SIZE

GMP_LIMB_BOUND : Integer
GMP_LIMB_BOUND = (1 `prim__shl_Integer` (GMP_LIMB_BITS))

-- To estimate required count of limbs (upper bound):
-- x = the number (result)
--   length of string == number of digits <= log10(x) + 1
--   required limbs: log10(x) / log10(2^LIMB_BITS)
--     LIMB_BITS=32 -> required limbs <= number of digits / 9
--     LIMB_BITS=64 -> required limbs <= number of digits / 19
GMP_ESTIMATE_DIGITS_PER_LIMB : Integer
GMP_ESTIMATE_DIGITS_PER_LIMB = 19

twosComplement : Num a => Bits a => a -> a
twosComplement x = 1 + (complement x)

export
cgMkConstInteger : Int -> Integer -> Codegen (IRValue IRObjPtr)
cgMkConstInteger i val =
    do
      let absVal = abs val
      let (len ** limbs) = getLimbs absVal
      let len32 = cast {to=Bits32} $ cast {to=Int} len
      let lenForHeader = if (val >= 0) then len32 else (twosComplement len32)
      let newHeader = constHeader OBJECT_TYPE_ID_BIGINT lenForHeader
      let typeSignature = "{i64, [" ++ show len ++ " x %LimbT]}"
      cName <- addConstant $ "private unnamed_addr addrspace(1) constant " ++ typeSignature ++ " {" ++ toIR newHeader ++ ", [" ++ show len ++ " x %LimbT] [" ++ (getLimbsIR limbs) ++ "]}, align 8"
      pure $ SSA IRObjPtr $ "bitcast (" ++ typeSignature ++ " addrspace(1)* " ++ cName ++ " to %ObjPtr)"
  where
      getLimbs : Integer -> (n:Nat ** Vect n Integer)
      getLimbs 0 = (0 ** [])
      getLimbs x = let (n ** v) = (getLimbs (x `div` GMP_LIMB_BOUND))
                       limb = (x `mod` GMP_LIMB_BOUND) in
                       ((S n) ** (limb::v))
      getLimbsIR : Vect n Integer -> String
      getLimbsIR [] = ""
      getLimbsIR (limb::[]) = "%LimbT " ++ show limb
      getLimbsIR (limb::rest) = "%LimbT " ++ show limb ++ ", " ++ (getLimbsIR rest)

export
integer0 : Codegen (IRValue IRObjPtr)
integer0 = do
  newObj <- dynamicAllocate (Const I64 0)
  putObjectHeader newObj (constHeader OBJECT_TYPE_ID_BIGINT 0)
  pure newObj

export
cgMkIntegerSigned : IRValue I64 -> Codegen (IRValue IRObjPtr)
cgMkIntegerSigned val = do
  isNegative <- icmp "slt" val (Const I64 0)
  isZero <- icmp "eq" val (Const I64 0)
  newSize1 <- mkSelect isNegative (Const I32 (-1)) (Const I32 1)
  newSize <- mkSelect isZero (Const I32 0) newSize1
  newHeader <- mkHeader OBJECT_TYPE_ID_BIGINT newSize
  newSizeAbs <- mkAbs newSize
  allocSize <- mkMul !(mkZext newSizeAbs) (Const I64 GMP_LIMB_SIZE)
  newObj <- dynamicAllocate allocSize
  ignore $ mkIf (pure isZero) (pure (Const I1 0)) (do
       absVal <- mkAbs64 val
       putObjectSlot newObj (Const I64 0) absVal
       pure $ Const I1 0)
  putObjectHeader newObj newHeader
  pure newObj

export
cgMkIntegerUnsigned : IRValue I64 -> Codegen (IRValue IRObjPtr)
cgMkIntegerUnsigned ival = do
  isZero <- icmp "eq" (Const I64 0) ival
  newObj <- mkIf (pure isZero) integer0 (do
      newInteger <- dynamicAllocate (Const I64 GMP_LIMB_SIZE)
      putObjectHeader newInteger !(mkHeader OBJECT_TYPE_ID_BIGINT (Const I32 1))
      putObjectSlot newInteger (Const I64 0) ival
      pure newInteger
    )
  pure newObj

export
unboxIntegerUnsigned : IRValue IRObjPtr -> Codegen (IRValue I64)
unboxIntegerUnsigned integerObj = do
  isZero <- icmp "eq" (Const I32 0) !(getObjectSize integerObj)
  -- get first limb (LSB)
  mkIf (pure isZero) (pure $ Const I64 0) (getObjectSlot {t=I64} integerObj 0)

export
unboxIntegerSigned : IRValue IRObjPtr -> Codegen (IRValue I64)
unboxIntegerSigned integerObj = do
  size <- getObjectSize integerObj
  isZero <- icmp "eq" (Const I32 0) size
  let isNegative = icmp "sgt" (Const I32 0) size
  -- get first limb (LSB)
  firstLimb <- getObjectSlot {t=I64} integerObj 0
  -- TODO: this is probably wrong for 64bit
  mkIf (pure isZero) (pure $ Const I64 0) (mkIf isNegative (mkSub (Const I64 0) firstLimb) (pure firstLimb))

||| compare two BigInts `a` and `b`, return -1 if a<b, +1 if a>b, 0 otherwise
export
compareInteger : IRValue IRObjPtr -> IRValue IRObjPtr -> Codegen (IRValue I64)
compareInteger obj1 obj2 = do
  size1 <- getObjectSize obj1
  size2 <- getObjectSize obj2
  cmpResult <- mkIf (icmp "slt" size1 size2) (pure (Const I64 (-1))) (
    mkIf (icmp "sgt" size1 size2) (pure (Const I64 1)) (do
         limbs1 <- getObjectPayloadAddr {t=I64} obj1
         limbs2 <- getObjectPayloadAddr {t=I64} obj2
         absSize <- mkZext {to=I64} !(mkAbs size1)
         mpnResult <- call {t=I32} "ccc" "@__gmpn_cmp" [toIR limbs1, toIR limbs2, toIR absSize]
         sizeIsNegative <- icmp "slt" size1 (Const I32 0)
         mkSext !(mkSelect sizeIsNegative !(mkSub (Const I32 0) mpnResult) mpnResult)
         )
         )

  pure cmpResult

normaliseIntegerSize : IRValue IRObjPtr -> IRValue I32 -> IRValue I1 -> Codegen ()
normaliseIntegerSize integerObj maxSizeSigned invert = do
  maxSizeAbs <- mkAbs maxSizeSigned
  absRealNewSize <- mkTrunc {to=I32} !(call {t=I64} "ccc" "@rapid_bigint_real_size" [
    toIR !(getObjectPayloadAddr {t=I64} integerObj),
    toIR !(mkZext {to=I64} maxSizeAbs)
    ])
  isNegative <- icmp "slt" maxSizeSigned (Const I32 0)
  invertResult <- mkXOr isNegative invert
  signedNewSize <- mkSelect invertResult !(mkSub (Const I32 0) absRealNewSize) absRealNewSize
  newHeader <- mkHeader OBJECT_TYPE_ID_BIGINT signedNewSize
  putObjectHeader integerObj newHeader

export
addInteger : IRValue IRObjPtr -> IRValue IRObjPtr -> Codegen (IRValue IRObjPtr)
addInteger i1 i2 = do
      s1 <- getObjectSize i1
      s2 <- getObjectSize i2
      i1Negative <- icmp "slt" s1 (Const I32 0)
      s1a <- mkAbs s1
      s2a <- mkAbs s2
      i1longer <- icmp "ugt" s1a s2a
      -- "big" and "small" refer just to the respective limb counts
      -- it doesn't matter which number is actually bigger
      big <- mkSelect i1longer i1 i2
      small <- mkSelect i1longer i2 i1
      size1 <- mkZext {to=I64} !(mkSelect {t=I32} i1longer s1a s2a)
      size2 <- mkZext {to=I64} !(mkSelect {t=I32} i1longer s2a s1a)
      newLength <- mkAdd size1 (Const I64 1)
      newSize <- mkMul (Const I64 GMP_LIMB_SIZE) newLength
      newObj <- dynamicAllocate newSize
      carry <- call {t=I64} "ccc" "@__gmpn_add" [
        toIR !(getObjectPayloadAddr {t=I64} newObj),
        toIR !(getObjectPayloadAddr {t=I64} big),
        toIR size1,
        toIR !(getObjectPayloadAddr {t=I64} small),
        toIR size2
        ]
      putObjectSlot newObj size1 carry
      absRealNewSize <- mkAdd size1 carry
      signedNewSize <- mkSelect i1Negative !(mkSub (Const I64 0) absRealNewSize) absRealNewSize
      signedNewSize32 <- mkTrunc {to=I32} signedNewSize
      newHeader <- mkHeader OBJECT_TYPE_ID_BIGINT signedNewSize32
      putObjectHeader newObj newHeader
      pure newObj

export
subInteger : IRValue IRObjPtr -> IRValue IRObjPtr -> Codegen (IRValue IRObjPtr)
subInteger i1 i2 = do
      -- Subtract the smaller (by abs. value) from the larger (by abs. value)
      -- and use the sign of the larger (by abs. value) number as sign for the
      -- returned result.
      s1 <- getObjectSize i1
      s2 <- getObjectSize i2
      i1Negative <- icmp "slt" s1 (Const I32 0)
      s1a <- mkAbs s1
      s2a <- mkAbs s2
      i1longer <- icmp "ugt" s1a s2a
      i2longer <- icmp "ugt" s2a s1a
      i1bigger <- mkIf (pure i1longer) (pure $ Const I1 1) (mkIf (pure i2longer) (pure $ Const I1 0) (icmp "sgt" !(call "ccc" "@__gmpn_cmp" [
                                toIR !(getObjectPayloadAddr {t=I64} i1),
                                toIR !(getObjectPayloadAddr {t=I64} i2),
                                toIR !(mkZext {to=I64} s1a)
                                ]) (Const I32 0))
        )
      big <- mkSelect i1bigger i1 i2
      small <- mkSelect i1bigger i2 i1
      swapped <- mkSelect i1bigger (Const I1 0) (Const I1 1)
      bigSize <- getObjectSize big
      bigSizeAbs <- mkAbs bigSize
      smallSizeAbs <- mkAbs !(getObjectSize small)
      newSize <- mkMul (Const I64 GMP_LIMB_SIZE) !(mkZext {to=I64} bigSizeAbs)
      newObj <- dynamicAllocate newSize
      absDiff <- call {t=I64} "ccc" "@__gmpn_sub" [
        toIR !(getObjectPayloadAddr {t=I64} newObj),
        toIR !(getObjectPayloadAddr {t=I64} big),
        toIR !(mkZext {to=I64} bigSizeAbs),
        toIR !(getObjectPayloadAddr {t=I64} small),
        toIR !(mkZext {to=I64} smallSizeAbs)
        ]
      absRealNewSize <- call {t=I64} "ccc" "@rapid_bigint_real_size" [
        toIR !(getObjectPayloadAddr {t=I64} newObj),
        toIR !(mkZext {to=I64} bigSizeAbs)
        ]
      resultIsNegative <- mkXOr swapped i1Negative
      signedNewSize <- mkSelect resultIsNegative !(mkSub (Const I64 0) absRealNewSize) absRealNewSize
      signedNewSize32 <- mkTrunc {to=I32} signedNewSize
      newHeader <- mkHeader OBJECT_TYPE_ID_BIGINT signedNewSize32
      putObjectHeader newObj newHeader
      pure newObj

export
andInteger : IRValue IRObjPtr -> IRValue IRObjPtr -> Codegen (IRValue IRObjPtr)
andInteger i1 i2 = do
  -- TODO: what to do with negative numbers?
  s1 <- getObjectSize i1
  s2 <- getObjectSize i2
  zero1 <- icmp "eq" s1 (Const I32 0)
  zero2 <- icmp "eq" s2 (Const I32 0)
  resultIsZero <- mkOr zero1 zero2

  mkIf (pure resultIsZero) (mkSelect zero1 i1 i2) (do
    s1a <- mkAbs s1
    s2a <- mkAbs s2
    i1longer <- icmp "ugt" s1a s2a
    -- "long" and "short" refer just to the respective limb counts
    -- it doesn't matter which number is actually bigger
    long <- mkSelect i1longer i1 i2
    short <- mkSelect i1longer i2 i1
    size1 <- mkZext {to=I64} !(mkSelect {t=I32} i1longer s1a s2a)
    size2 <- mkZext {to=I64} !(mkSelect {t=I32} i1longer s2a s1a)
    -- result can not be longer than shortest number
    let newLength = size2
    newSize <- mkMul (Const I64 GMP_LIMB_SIZE) newLength
    newObj <- dynamicAllocate newSize
    putObjectHeader newObj !(mkHeader OBJECT_TYPE_ID_BIGINT !(mkTrunc newLength))

    newLimbs <- getObjectPayloadAddr {t=I64} newObj
    shortLimbs <- getObjectPayloadAddr {t=I64} short
    longLimbs <- getObjectPayloadAddr {t=I64} long
    voidCall "ccc" "@__gmpn_and_n" [toIR newLimbs, toIR shortLimbs, toIR longLimbs, toIR newLength]

    normaliseIntegerSize newObj !(mkTrunc newLength) (Const I1 0)

    pure newObj
    )

export
orInteger : IRValue IRObjPtr -> IRValue IRObjPtr -> Codegen (IRValue IRObjPtr)
orInteger i1 i2 = do
  -- TODO: what to do with negative numbers?
  s1 <- getObjectSize i1
  s2 <- getObjectSize i2
  zero1 <- icmp "eq" s1 (Const I32 0)
  zero2 <- icmp "eq" s2 (Const I32 0)
  resultIsZero <- mkAnd zero1 zero2

  mkIf (pure resultIsZero) (pure i1) (do
    s1a <- mkAbs s1
    s2a <- mkAbs s2
    i1longer <- icmp "ugt" s1a s2a
    -- "big" and "small" refer just to the respective limb counts
    -- it doesn't matter which number is actually bigger
    big <- mkSelect i1longer i1 i2
    small <- mkSelect i1longer i2 i1
    size1 <- mkZext {to=I64} !(mkSelect {t=I32} i1longer s1a s2a)
    size2 <- mkZext {to=I64} !(mkSelect {t=I32} i1longer s2a s1a)
    mkIf (icmp "eq" (Const I64 0) size2) (pure big) (do
      let newLength = size1
      newSize <- mkMul (Const I64 GMP_LIMB_SIZE) newLength
      newObj <- dynamicAllocate newSize
      putObjectHeader newObj !(mkHeader OBJECT_TYPE_ID_BIGINT !(mkTrunc newLength))

      newPayload <- getObjectPayloadAddr {t=I8} newObj
      bigPayload <- getObjectPayloadAddr {t=I8} big
      appendCode $ "  call void @llvm.memcpy.p1i8.p1i8.i64(" ++ toIR newPayload ++ ", " ++ toIR bigPayload ++ ", " ++ toIR newSize++ ", i1 false)"

      newLimbs <- getObjectPayloadAddr {t=I64} newObj
      smallLimbs <- getObjectPayloadAddr {t=I64} small
      voidCall "ccc" "@__gmpn_ior_n" [toIR newLimbs, toIR newLimbs, toIR smallLimbs, toIR size2]
      pure newObj
      )
    )

export
xorInteger : IRValue IRObjPtr -> IRValue IRObjPtr -> Codegen (IRValue IRObjPtr)
xorInteger i1 i2 = do
  -- TODO: what to do with negative numbers?
  s1 <- getObjectSize i1
  s2 <- getObjectSize i2
  zero1 <- icmp "eq" s1 (Const I32 0)
  zero2 <- icmp "eq" s2 (Const I32 0)
  resultIsUnchanged <- mkOr zero1 zero2

  mkIf (pure resultIsUnchanged) (mkSelect zero1 i2 i1) (do
    s1a <- mkAbs s1
    s2a <- mkAbs s2
    i1longer <- icmp "ugt" s1a s2a
    -- "long" and "short" refer just to the respective limb counts
    -- it doesn't matter which number is actually bigger
    long <- mkSelect i1longer i1 i2
    short <- mkSelect i1longer i2 i1
    size1 <- mkZext {to=I64} !(mkSelect {t=I32} i1longer s1a s2a)
    size2 <- mkZext {to=I64} !(mkSelect {t=I32} i1longer s2a s1a)
    -- result can not be longer than longest number
    let newLength = size1
    newSize <- mkMul (Const I64 GMP_LIMB_SIZE) newLength
    newObj <- dynamicAllocate newSize
    putObjectHeader newObj !(mkHeader OBJECT_TYPE_ID_BIGINT !(mkTrunc newLength))

    newPayload <- getObjectPayloadAddr {t=I8} newObj
    longPayload <- getObjectPayloadAddr {t=I8} long
    appendCode $ "  call void @llvm.memcpy.p1i8.p1i8.i64(" ++ toIR newPayload ++ ", " ++ toIR longPayload ++ ", " ++ toIR newSize ++ ", i1 false)"

    newLimbs <- getObjectPayloadAddr {t=I64} newObj
    shortLimbs <- getObjectPayloadAddr {t=I64} short
    voidCall "ccc" "@__gmpn_xor_n" [toIR newLimbs, toIR newLimbs, toIR shortLimbs, toIR size2]

    normaliseIntegerSize newObj !(mkTrunc newLength) (Const I1 0)

    pure newObj
    )

export
mulInteger : IRValue IRObjPtr -> IRValue IRObjPtr -> Codegen (IRValue IRObjPtr)
mulInteger i1 i2 = do
      s1 <- getObjectSize i1
      s2 <- getObjectSize i2
      zero1 <- icmp "eq" s1 (Const I32 0)
      zero2 <- icmp "eq" s2 (Const I32 0)
      resultIsZero <- mkOr zero1 zero2
      mkIf (pure resultIsZero) {- then -} integer0 {- else -} (do
        sx <- mkXOr s1 s2
        signsMatch <- icmp "sge" sx (Const I32 0)
        s1a <- mkAbs s1
        s2a <- mkAbs s2
        i1longer <- icmp "ugt" s1a s2a
        -- "big" and "small" refer just to the respective limb counts
        -- it doesn't matter which number is actually bigger
        big <- mkSelect i1longer i1 i2
        small <- mkSelect i1longer i2 i1
        size1 <- mkZext {to=I64} !(mkSelect {t=I32} i1longer s1a s2a)
        size2 <- mkZext {to=I64} !(mkSelect {t=I32} i1longer s2a s1a)
        newLength <- mkAdd size1 size2
        newSize <- mkMul (Const I64 GMP_LIMB_SIZE) newLength
        newObj <- dynamicAllocate newSize
        ignore $ call {t=I64} "ccc" "@__gmpn_mul" [
          toIR !(getObjectPayloadAddr {t=I64} newObj),
          toIR !(getObjectPayloadAddr {t=I64} big),
          toIR size1,
          toIR !(getObjectPayloadAddr {t=I64} small),
          toIR size2
          ]
        absRealNewSize <- call {t=I64} "ccc" "@rapid_bigint_real_size" [
          toIR !(getObjectPayloadAddr {t=I64} newObj),
          toIR newLength
          ]
        signedNewSize <- mkSelect signsMatch absRealNewSize !(mkSub (Const I64 0) absRealNewSize)
        signedNewSize32 <- mkTrunc {to=I32} signedNewSize
        newHeader <- mkHeader OBJECT_TYPE_ID_BIGINT signedNewSize32
        putObjectHeader newObj newHeader
        pure newObj)

||| divide i1 by i2, return (quotient, remainder)
divInteger : IRValue IRObjPtr -> IRValue IRObjPtr -> Codegen (IRValue IRObjPtr, IRValue IRObjPtr)
divInteger i1 i2 = do
  s1 <- getObjectSize i1
  s2 <- getObjectSize i2
  s1a <- mkZext !(mkAbs s1)
  s2a <- mkZext !(mkAbs s2)
  zero1 <- icmp "eq" s1 (Const I32 0)
  zero2 <- icmp "eq" s2 (Const I32 0)
  ignore $ mkIf (pure zero2) (do
                mkRuntimeCrash "division by 0"
                pure (Const I1 0)
                ) (pure (Const I1 0))

  retZeroLbl <- genLabel "ret0"
  checkDividendLbl <- genLabel "div_chk"
  dividendLargerLbl <- genLabel "div_lg"
  divLbl <- genLabel "div"
  endLbl <- genLabel "div_end"

  branch zero1 retZeroLbl checkDividendLbl

  beginLabel retZeroLbl
  zeroInteger <- integer0
  jump endLbl

  beginLabel checkDividendLbl
  dividendLarger <- icmp "ugt" s2a s1a
  branch dividendLarger dividendLargerLbl divLbl

  beginLabel dividendLargerLbl
  zeroQuotient <- integer0
  jump endLbl

  beginLabel divLbl
  -- i1, i2 /= 0
  sx <- mkXOr s1 s2
  signsMatch <- icmp "sge" sx (Const I32 0)

  -- remainder can not be bigger than divisor
  let maxLimbsRemainder = s2a
  remainder <- dynamicAllocate !(mkMul (Const I64 GMP_LIMB_SIZE) maxLimbsRemainder)
  -- object must have a valid header, because the next allocation might trigger a GC
  tempHeader <- mkHeader OBJECT_TYPE_ID_BIGINT !(mkTrunc maxLimbsRemainder)
  putObjectHeader remainder tempHeader

  maxLimbsQuotient <- mkMax (Const I64 1) !(mkAdd (Const I64 1) !(mkSub s1a s2a))
  quotient <- dynamicAllocate !(mkMul (Const I64 GMP_LIMB_SIZE) maxLimbsQuotient)

  voidCall "ccc" "@__gmpn_tdiv_qr" [
    toIR !(getObjectPayloadAddr {t=I64} quotient),
    toIR !(getObjectPayloadAddr {t=I64} remainder),
    toIR (Const I64 0),
    toIR !(getObjectPayloadAddr {t=I64} i1),
    toIR s1a,
    toIR !(getObjectPayloadAddr {t=I64} i2),
    toIR s2a
    ]
  qRealNewSize <- call {t=I64} "ccc" "@rapid_bigint_real_size" [
    toIR !(getObjectPayloadAddr {t=I64} quotient),
    toIR maxLimbsQuotient
    ]
  signedNewSize <- mkSelect signsMatch qRealNewSize !(mkSub (Const I64 0) qRealNewSize)
  signedNewSize32 <- mkTrunc {to=I32} signedNewSize
  newHeader <- mkHeader OBJECT_TYPE_ID_BIGINT signedNewSize32
  putObjectHeader quotient newHeader

  i1negative <- icmp "slt" s1 (Const I32 0)
  rRealNewSize <- call {t=I64} "ccc" "@rapid_bigint_real_size" [
    toIR !(getObjectPayloadAddr {t=I64} remainder),
    toIR maxLimbsRemainder
    ]
  signedNewSize <- mkSelect i1negative !(mkSub (Const I64 0) rRealNewSize) rRealNewSize
  signedNewSize32 <- mkTrunc {to=I32} signedNewSize
  newHeader <- mkHeader OBJECT_TYPE_ID_BIGINT signedNewSize32
  putObjectHeader remainder newHeader

  jump endLbl

  beginLabel endLbl
  quotient  <- phi [(zeroInteger, retZeroLbl), (zeroQuotient, dividendLargerLbl), (quotient,  divLbl)]
  remainder <- phi [(zeroInteger, retZeroLbl), (i1,           dividendLargerLbl), (remainder, divLbl)]
  pure (quotient, remainder)

export
divIntegerQuotient : IRValue IRObjPtr -> IRValue IRObjPtr -> Codegen (IRValue IRObjPtr)
divIntegerQuotient a b = fst <$> divInteger a b

export
divIntegerRemainder : IRValue IRObjPtr -> IRValue IRObjPtr -> Codegen (IRValue IRObjPtr)
divIntegerRemainder a b = snd <$> divInteger a b

export
shiftLeftInteger : IRValue IRObjPtr -> IRValue IRObjPtr -> Codegen (IRValue IRObjPtr)
shiftLeftInteger integerObj bitCountObj = do
  bitCount <- mkTrunc {to=I32} !(unboxIntegerUnsigned bitCountObj)

  size <- getObjectSize integerObj
  unchanged <- mkOr !(icmp "eq" size (Const I32 0)) !(icmp "eq" bitCount (Const I32 0))
  mkIf (pure unchanged) (do
    pure integerObj
    ) (do
    sizeAbs <- mkAbs size
    fullLimbs <- mkUDiv bitCount (Const I32 GMP_LIMB_BITS)
    maxLimbsCount <- mkAdd !(mkAdd fullLimbs sizeAbs) (Const I32 1)

    newObj <- dynamicAllocate !(mkZext !(mkMul maxLimbsCount (Const I32 GMP_LIMB_SIZE)))
    lowerLimbsAddr <- getObjectPayloadAddr {t=I8} newObj
    appendCode $ "  call void @llvm.memset.p1i8.i64(" ++ toIR lowerLimbsAddr ++ ", i8 0, " ++ toIR !(mkMul !(mkZext fullLimbs) (Const I64 8)) ++ ", i1 false)"

    restBits <- mkURem bitCount (Const I32 GMP_LIMB_BITS)
    mkIf_ (icmp "ne" (Const I32 0) restBits) (do
      srcLimbs <- getObjectPayloadAddr {t=I64} integerObj
      higherLimbsAddr <- getObjectSlotAddrVar {t=I64} newObj !(mkZext {to=I64} fullLimbs)
      msbLimb <- call {t=I64} "ccc" "@__gmpn_lshift" [
        toIR higherLimbsAddr,
        toIR srcLimbs,
        toIR !(mkZext {to=I64} sizeAbs),
        toIR restBits
        ]
      msbLimbAddr <- getObjectSlotAddrVar {t=I64} newObj !(mkZext !(mkSub maxLimbsCount (Const I32 1)))
      store msbLimb msbLimbAddr
      ) (do
      srcLimbs <- getObjectPayloadAddr {t=I8} integerObj
      higherLimbsAddr <- getObjectSlotAddrVar {t=I8} newObj !(mkZext {to=I64} fullLimbs)
      voidCall "ccc" "@llvm.memcpy.p1i8.p1i8.i64" [
        toIR higherLimbsAddr,
        toIR srcLimbs,
        toIR !(mkMul !(mkZext size) (Const I64 GMP_LIMB_SIZE)),
        "i1 false"
        ]
      )

    isNegative <- icmp "slt" size (Const I32 0)
    normaliseIntegerSize newObj maxLimbsCount isNegative
    pure newObj
    )

export
shiftRightInteger : IRValue IRObjPtr -> IRValue IRObjPtr -> Codegen (IRValue IRObjPtr)
shiftRightInteger integerObj bitCountObj = do
  bitCount <- mkTrunc {to=I32} !(unboxIntegerUnsigned bitCountObj)

  size <- getObjectSize integerObj
  unchanged <- mkOr !(icmp "eq" size (Const I32 0)) !(icmp "eq" bitCount (Const I32 0))
  mkIf (pure unchanged) (pure integerObj) (do
    sizeAbs <- mkAbs size
    fullLimbs <- mkUDiv bitCount (Const I32 GMP_LIMB_BITS)
    maxLimbsCount <- mkSub sizeAbs fullLimbs

    mkIf (icmp "sle" maxLimbsCount (Const I32 0)) integer0 (do
      newObj <- dynamicAllocate !(mkZext !(mkMul maxLimbsCount (Const I32 GMP_LIMB_SIZE)))

      restBits <- mkURem bitCount (Const I32 GMP_LIMB_BITS)
      mkIf_ (icmp "ne" (Const I32 0) restBits) (do
        srcHigherLimbs <- getObjectSlotAddrVar {t=I64} integerObj !(mkZext {to=I64} fullLimbs)
        dstLimbsAddr <- getObjectPayloadAddr {t=I64} newObj
        ignore $ call {t=I64} "ccc" "@__gmpn_rshift" [
          toIR dstLimbsAddr,
          toIR srcHigherLimbs,
          toIR !(mkZext {to=I64} maxLimbsCount),
          toIR restBits
          ]
        ) (do
        srcHigherLimbs <- getObjectSlotAddrVar {t=I8} integerObj !(mkZext {to=I64} fullLimbs)
        dstLimbsAddr <- getObjectPayloadAddr {t=I8} newObj
        voidCall "ccc" "@llvm.memcpy.p1i8.p1i8.i64" [
          toIR dstLimbsAddr,
          toIR srcHigherLimbs,
          toIR !(mkMul !(mkZext maxLimbsCount) (Const I64 GMP_LIMB_SIZE)),
          "i1 false"
          ]
        )

      isNegative <- icmp "slt" size (Const I32 0)
      normaliseIntegerSize newObj maxLimbsCount isNegative
      pure newObj
      )

    )

IEEE_DOUBLE_MASK_EXP  : Bits64
IEEE_DOUBLE_MASK_EXP  = 0x7ff0000000000000
IEEE_DOUBLE_MASK_FRAC : Bits64
IEEE_DOUBLE_MASK_FRAC = 0x000fffffffffffff
IEEE_DOUBLE_MASK_SIGN : Bits64
IEEE_DOUBLE_MASK_SIGN = 0x8000000000000000
IEEE_DOUBLE_INF_POS   : Bits64
IEEE_DOUBLE_INF_POS   = 0x7ff0000000000000
IEEE_DOUBLE_INF_NEG   : Bits64
IEEE_DOUBLE_INF_NEG   = 0xfff0000000000000

export
castDoubleToInteger : IRValue IRObjPtr -> Codegen (IRValue IRObjPtr)
castDoubleToInteger floatObj = do
  floatBitsAsI64 <- getObjectSlot {t=I64} floatObj 0
  exponent <- mkShiftR !(mkAnd (Const I64 $ cast IEEE_DOUBLE_MASK_EXP) floatBitsAsI64) (Const I64 52)
  -- NaN and infinity will be returned as "0"
  isInfOrNaN <- icmp "eq" exponent (Const I64 0x7ff)
  -- absolute values < 1.0 will be returned as "0"
  isSmallerThanOne <- icmp "ult" exponent (Const I64 1023)
  returnZero <- mkOr isInfOrNaN isSmallerThanOne

  mkIf (pure returnZero) {- then -} integer0 {- else -} (do
    fraction <- mkAnd (Const I64 $ cast IEEE_DOUBLE_MASK_FRAC) floatBitsAsI64
    -- highest bit is sign bit in both, Double and I64, so we can just use that one
    isNegative <- icmp "slt" floatBitsAsI64 (Const I64 0)
    initial <- mkOr fraction (Const I64 0x10000000000000)
    toShift <- mkSub exponent (Const I64 1075)
    shiftLeft <- icmp "sgt" toShift (Const I64 0)
    mkIf (pure shiftLeft) (do
      let maxLimbCount = Const I64 17
      payloadSize <- mkMul maxLimbCount (Const I64 GMP_LIMB_SIZE)
      -- requiredBits <- (exponent - 1022)
      -- requiredLimbs <- (requiredBits+63) / 64
      -- newObj <- allocObject (requiredLimbs * 8)
      newObj <- dynamicAllocate payloadSize
      payloadAddr <- getObjectPayloadAddr {t=I8} newObj
      appendCode $ "  call void @llvm.memset.p1i8.i64(" ++ toIR payloadAddr ++ ", i8 0, " ++ toIR payloadSize ++ ", i1 false)"
      putObjectSlot newObj (Const I64 0) initial
      absRealNewSize <- call {t=I64} "ccc" "@rapid_bigint_lshift_inplace" [
        toIR !(getObjectPayloadAddr {t=I64} newObj),
        toIR maxLimbCount,
        toIR !(mkTrunc {to=I32} toShift)
      ]
      signedNewSize <- mkSelect isNegative !(mkSub (Const I64 0) absRealNewSize) absRealNewSize
      size32 <- mkTrunc {to=I32} signedNewSize
      newHeader <- mkHeader OBJECT_TYPE_ID_BIGINT size32
      putObjectHeader newObj newHeader
      pure newObj
      ) (do
        newObj <- dynamicAllocate (Const I64 8)
        toShiftRight <- mkSub (Const I64 0) toShift
        shifted <- mkShiftR initial toShiftRight
        putObjectSlot newObj (Const I64 0) shifted
        signedNewSize32 <- mkSelect isNegative (Const I32 (-1)) (Const I32 1)
        newHeader <- mkHeader OBJECT_TYPE_ID_BIGINT signedNewSize32
        putObjectHeader newObj newHeader
        pure newObj
      )
    )

export
castIntegerToDouble : IRValue IRObjPtr -> Codegen (IRValue IRObjPtr)
castIntegerToDouble intObj = do
  size <- getObjectSize intObj
  isZero <- icmp "eq" (Const I32 0) size

  mkIf (pure isZero) (cgMkConstDouble 0.0) (do
    sizeAbs <- mkAbs size
    highestLimbIndex <- mkZext {to=I64} !(mkSub sizeAbs (Const I32 1))
    msbLimbAddr <- getObjectSlotAddrVar {t=I64} intObj highestLimbIndex
    msbLimb <- load msbLimbAddr
    countLeadingZeros <- SSA I64 <$> assignSSA ("  call ccc i64 @llvm.ctlz.i64(" ++ toIR msbLimb ++ ", i1 1)")
    exponentOffset <- mkAdd (Const I64 (1023 + 63)) !(mkMul highestLimbIndex (Const I64 64))
    exponent <- mkSub exponentOffset countLeadingZeros

    isInfinity <- icmp "ugt" exponent (Const I64 2046)
    doubleAsBits <- mkIf (pure isInfinity) (do
        pure (Const I64 $ cast IEEE_DOUBLE_INF_POS)
      ) (do
        fracShiftLeft <- icmp "uge" countLeadingZeros (Const I64 12)
        shiftedFraction <- mkIf (pure fracShiftLeft) (do
            mkShiftL msbLimb !(mkSub countLeadingZeros (Const I64 11))
          ) (do
            mkShiftR msbLimb !(mkSub (Const I64 11) countLeadingZeros)
          )
        fraction <- mkIf (icmp "eq" sizeAbs (Const I32 1)) (do
            pure shiftedFraction
          ) (do
            mkIf (pure fracShiftLeft) (do
                secondMsbLimbAddr <- getObjectSlotAddrVar {t=I64} intObj !(mkSub highestLimbIndex (Const I64 1))
                secondMsbLimb <- load secondMsbLimbAddr
                fractionLowerPart <- mkShiftR secondMsbLimb !(mkSub (Const I64 (64 + 11)) countLeadingZeros)
                mkOr shiftedFraction fractionLowerPart
              ) (do
                pure shiftedFraction
              )
          )
        shiftedExponent <- mkShiftL exponent (Const I64 52)
        maskedFraction <- mkAnd (Const I64 $ cast IEEE_DOUBLE_MASK_FRAC) fraction
        mkOr shiftedExponent maskedFraction
      )
    isNegative <- icmp "slt" size (Const I32 0)
    sign <- mkSelect isNegative (Const I64 $ cast IEEE_DOUBLE_MASK_SIGN) (Const I64 0)
    signedDoubleAsBits <- mkOr sign doubleAsBits
    cgMkDoubleFromBits signedDoubleAsBits
    )

export
castStringToInteger : IRValue IRObjPtr -> Codegen (IRValue IRObjPtr)
castStringToInteger strObj = do
  strLength <- getObjectSize strObj
  maxLimbsCount <- mkUDiv strLength (Const I32 GMP_ESTIMATE_DIGITS_PER_LIMB)
  -- GMP requires 1 limb scratch space
  maxLimbsCountPlus1 <- mkAdd maxLimbsCount (Const I32 1)

  newObj <- dynamicAllocate !(mkZext !(mkMul maxLimbsCountPlus1 (Const I32 GMP_LIMB_SIZE)))
  putObjectHeader newObj !(mkHeader OBJECT_TYPE_ID_BIGINT maxLimbsCountPlus1)
  ignore $ call {t=I32} "ccc" "@rapid_bigint_set_str" [
    toIR newObj,
    toIR strObj
    ]
  pure newObj
