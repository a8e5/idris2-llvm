-- the commented-out cases are still wrong,
-- but fixing them as well would make other tests fail for mysterious reasons
-- see https://github.com/edwinb/Idris2/pull/281
main : IO ()
main = do
  printLn $ 3
  printLn $ 4.2
  printLn $ "1.2"

  printLn $ cast {to = Int} 4.8
  printLn $ cast {to = Integer} 5.0e100
  printLn $ cast {to = Integer} (-5.0e100)
  printLn $ cast {to = Integer} 1.2
  printLn $ cast {to = Integer} 0.0
  printLn $ (cast {to = Integer} 0.0) == (the Integer 0)
  printLn $ cast {to = Integer} (-13374242.0)
  printLn $ cast {to = String} 2.7

  -- printLn $ cast {to = Int} "1.2"
  printLn $ cast {to = Integer} "2"
  printLn $ cast {to = Integer} "133742420000000006660000000000000000000013000000000000700000000000002"
  printLn $ cast {to = Integer} "000000000000000000000000000000000000000000000000000000000000"
  printLn $ cast {to = Integer} "0"
  printLn $ cast {to = Integer} "-0"
  printLn $ cast {to = Integer} "-1024"
  printLn $ cast {to = Integer} "-0001938712097836128076319208763102"
  printLn $ cast {to = Double} "5.9"

  printLn $ (the Int 6 `div` the Int 3)
  printLn $ (the Integer 6 `div` the Integer 3)
  printLn $ (cast {to = Int} "6.6" `div` cast "3.9")
  printLn $ (cast {to = Double} (the Int 13374242))
  putStrLn "cast Integer->Double:"
  printLn $ (cast {to = Double} (the Integer 0))
  printLn $ (cast {to = Double} (the Integer 0xffffffffffffffff))
  printLn $ (cast {to = Double} (the Integer 13374242))
  printLn $ (cast {to = Double} (the Integer (-13374242)))
  printLn $ (cast {to = Double} (the Integer 13374242000000000000000000000000000000000000000000000000000000000000))
  printLn $ (cast {to = Double} (the Integer 0xffffffffffffffff0000000))
  -- 2^1023
  printLn $ (cast {to = Double} (the Integer 0x8000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000))
  -- 2^1024
  printLn $ (cast {to = Double} (the Integer 0x10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000))
  -- -2^1023
  printLn $ (cast {to = Double} (the Integer $ -0x8000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000))
  -- -2^1024
  printLn $ (cast {to = Double} (the Integer $ -0x10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000))
  printLn $ the Double (3.14159265 + 7.0)
  printLn $ the Double (3.14159265 - 7.0)
  printLn $ the Double (3.14159265 * 7.0)
  printLn $ the Double (3.14159265 / 7.0)

  putStrLn "left is smaller"
  printLn (3.14159265 < 7.0)
  printLn (3.14159265 <= 7.0)
  printLn (3.14159265 == 7.0)
  printLn (3.14159265 >= 7.0)
  printLn (3.14159265 > 7.0)

  putStrLn "left is bigger"
  printLn (9.14159265 < 7.0)
  printLn (9.14159265 <= 7.0)
  printLn (9.14159265 == 7.0)
  printLn (9.14159265 >= 7.0)
  printLn (9.14159265 > 7.0)

  putStrLn "equal"
  printLn (9.14159265 <  9.14159265)
  printLn (9.14159265 <= 9.14159265)
  printLn (9.14159265 == 9.14159265)
  printLn (9.14159265 >= 9.14159265)
  printLn (9.14159265 >  9.14159265)
  -- printLn $ (cast {to = Integer} "6.6" `div` cast "3.9")
