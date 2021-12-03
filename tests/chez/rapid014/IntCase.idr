module Main

caseB8 : Bits8 -> String
caseB8 0 = "B8 - zero"
caseB8 42 = "B8 - answer"
caseB8 _ = "B8 - nonzero"

caseB16 : Bits16 -> String
caseB16 0 = "B16 - zero"
caseB16 42 = "B16 - answer"
caseB16 _ = "B16 - nonzero"

caseB32 : Bits32 -> String
caseB32 0 = "B32 - zero"
caseB32 42 = "B32 - answer"
caseB32 _ = "B32 - nonzero"

caseB64 : Bits64 -> String
caseB64 0 = "B64 - zero"
caseB64 42 = "B64 - answer"
caseB64 _ = "B64 - nonzero"

caseI8 : Bits8 -> String
caseI8 0 = "I8 - zero"
caseI8 42 = "I8 - answer"
caseI8 _ = "I8 - nonzero"

caseI16 : Bits16 -> String
caseI16 0 = "I16 - zero"
caseI16 42 = "I16 - answer"
caseI16 _ = "I16 - nonzero"

caseI32 : Bits32 -> String
caseI32 0 = "I32 - zero"
caseI32 42 = "I32 - answer"
caseI32 _ = "I32 - nonzero"

caseI64 : Bits64 -> String
caseI64 0 = "I64 - zero"
caseI64 42 = "I64 - answer"
caseI64 _ = "I64 - nonzero"

main : IO ()
main = do
  putStrLn $ caseB8 0
  putStrLn $ caseB8 (cast (the Bits64 0x1000002a))
  putStrLn $ caseB8 255

  putStrLn $ caseB16 0
  putStrLn $ caseB16 42
  putStrLn $ caseB16 255

  putStrLn $ caseB32 0
  putStrLn $ caseB32 42
  putStrLn $ caseB32 255

  putStrLn $ caseB64 0
  putStrLn $ caseB64 42
  putStrLn $ caseB64 255

  putStrLn $ caseI8 0
  putStrLn $ caseI8 (cast (the Bits64 0x1000002a))
  putStrLn $ caseI8 255

  putStrLn $ caseI16 0
  putStrLn $ caseI16 42
  putStrLn $ caseI16 255

  putStrLn $ caseI32 0
  putStrLn $ caseI32 42
  putStrLn $ caseI32 255

  putStrLn $ caseI64 0
  putStrLn $ caseI64 42
  putStrLn $ caseI64 255
