small : Int
small = 1024

integer : Integer
integer = 1024

-- 1337^42
big : Integer
big = 198389692832016689128025814051186435469808931027259980194805041212767924492279648804437095653839742535006120000819629040274718649969

-- 42^1337
huge : Integer
huge = 19244262046199218289931636915033148106690641506040881629783098396054017587189956898320826711503323817045899963919998763011641361593775134215582153498654561545030147946821842906655224585956346872948225465166671075178285211031263457874048501582084553256069820456514520740415585992986605588005867514603950159000316735374866963476435349611870363694882689627967863234398466592765982352187632158936137546106776134889060185573191536997089646035035586091184259132936730095612512733265705329122385839515503136560331820886633402814871252989377410963796772040968840920497661228445772698506863370746435110584336215353046341785122712126952997056649734116550162294539237838778195725728644670380926068202463208188658422788488542908663804399410181247832612673218863475183231178716370445215945708995057210985614406905891761439422370057500438135391252898524291272443916058253255186324146717892519879074789506310321409134984170936239885944193198276902040225295868483237495400052629350069690296661396970618075235332982265660277848285743630683658698739003163838964404283179484408396595263325214147203242500437826932246547465722961144585568761427992543192551906898277970979535170689620372910315316158639185570152860106155019655271591835540908899156136592672488480477903879058810734768809483078708839296938869838666010714053800464450222758046325085019284051676428041113338703481395786358804280910989969277918471493541307605061976779030467894334244626289114534954162063942686929523565378585575589036099169643448543590280539721343377840877466700634150681798382393646371307859860479924240964360139274558188148061851735039724582040226957180561368300949808838439429850254088330046438254337418859480029443981831309959042606279561201552047801023744751560073488524280982300361060330055022131244890702524234519256607737080520101700800111710873091223285735611928278687421952980832780475404481134195880420280308604040460964909040207019087650758705809544273374785580020484327263803252908616431006499151697050817572591457592534336233120764511418623892426882025189756673992441209649105961640536826966336674073816419423794186027482454174380775862560836939595203812810553698727471805180082798129694536527511552

lim64 : Integer
lim64 = 0xffffffffffffffff
lim63 : Integer
lim63 = 0x7fffffffffffffff

zero : Integer
zero = 0
five : Integer
five = 5
eight : Integer
eight = 8

negative : Integer
negative = -753

integer1Digit : Integer
integer1Digit = 1
integer2Digit : Integer
integer2Digit = 12
integer3Digit : Integer
integer3Digit = 123
integer4Digit : Integer
integer4Digit = 1234
integer5Digit : Integer
integer5Digit = 12345
integer6Digit : Integer
integer6Digit = 123456
integer7Digit : Integer
integer7Digit = 1234567
integer8Digit : Integer
integer8Digit = 12345678
integer9Digit : Integer
integer9Digit = 123456789

mult : IO ()
mult = do
  putStrLn "multiplication:"
  printLn (big * huge)
  printLn (huge * big)
  printLn (huge * 0)
  printLn (0 * big)

division : IO ()
division = do
  putStrLn "division:"
  printLn (huge `div` big)
  printLn (big `div` huge)
  printLn (huge `mod` big)
  printLn (big `mod` huge)

testShiftLeft : IO ()
testShiftLeft = do
  putStrLn "shiftLeft"
  printLn (1 `prim__shl_Integer` 63)
  printLn (1 `prim__shl_Integer` 64)
  printLn (0x1000000000000001 `prim__shl_Integer` 0)
  printLn (0x1000000000000001 `prim__shl_Integer` 1)
  printLn (0x1000000000000001 `prim__shl_Integer` 2)
  printLn (0x1000000000000001 `prim__shl_Integer` 63)
  printLn (0x1000000000000001 `prim__shl_Integer` 64)
  printLn (0x1000000000000001 `prim__shl_Integer` 65)
  printLn (0x1000000000000001 `prim__shl_Integer` 139)
  printLn (0x1000000000000001 `prim__shl_Integer` 140)
  printLn (0x1000000000000001 `prim__shl_Integer` 141)

testShiftRight : IO ()
testShiftRight = do
  putStrLn "shiftRight"
  printLn (1 `prim__shr_Integer` 0)
  printLn (1 `prim__shr_Integer` 1)
  printLn (0x8000000000000001 `prim__shr_Integer` 1)
  printLn (0x8000000000000001 `prim__shr_Integer` 63)
  printLn (0x8000000000000001 `prim__shr_Integer` 64)
  printLn (0x100000000000000100000000000000000000000000000000000000000000000000000000000000000 `prim__shr_Integer` 139)
  printLn (0x0 `prim__shr_Integer` 0)
  printLn (0x0 `prim__shr_Integer` 1)
  printLn (0xffffffffffffffffffffffffffffffffffffffffffffffffffffff `prim__shr_Integer` 99)
  printLn (0xffffffffffffffffffffffffffffffffffffffffffffffffffffff `prim__shr_Integer` 999999)

altbits : Integer
altbits = 0x55555555555555555555555555555555555555555555555555555555555555

thirdbits : Integer
thirdbits = 0x249249249249249249249249249249249249249249249249249249249249249249249249249249249249249249249249249249249249249249249249249249249249249249249249249249249249249249249249249249249249

main : IO ()
main = do
  mult
  division
  putStrLn "casts:"
  printLn $ (prim__cast_IntegerInt (natToInteger Z))
  printLn $ (prim__cast_IntegerInt (natToInteger (S Z)))
  printLn $ (prim__cast_IntInteger 0)
  printLn $ (prim__cast_IntInteger 1)
  printLn $ (prim__cast_IntInteger 2)
  printLn $ (prim__cast_IntegerInt (prim__cast_IntInteger 100))
  printLn $ (prim__cast_IntInteger 127)
  printLn $ (prim__cast_IntInteger (-1))
  printLn $ (prim__cast_IntInteger 0xff)
  printLn $ (prim__cast_IntInteger 0x100000)
  putStrLn "add0:"
  printLn $ (zero + zero)
  printLn $ (five + zero)
  printLn $ (zero + five)
  printLn $ (zero - five)
  printLn $ (five - zero)
  putStrLn "bitwise"
  printLn (altbits `prim__or_Integer` thirdbits)
  printLn (thirdbits `prim__or_Integer` altbits)
  printLn (altbits `prim__and_Integer` thirdbits)
  printLn (thirdbits `prim__and_Integer` altbits)
  printLn (altbits `prim__xor_Integer` thirdbits)
  printLn (thirdbits `prim__xor_Integer` altbits)
  printLn ((altbits `prim__xor_Integer` thirdbits) `prim__xor_Integer` altbits)
  printLn ((altbits `prim__xor_Integer` thirdbits) `prim__xor_Integer` thirdbits)
  testShiftLeft
  testShiftRight
  putStrLn "ok"
  putStrLn (show small)
  putStrLn (show $ cast {to=Int} integer)
  putStrLn (show $ cast {to=Int} (1024 `prim__add_Integer` 1024))
  putStrLn (show (1024 `prim__lt_Integer` 2048))
  putStrLn (show (1024 `prim__gt_Integer` 2048))
  putStrLn (show (the Integer 1337))
  putStrLn (show big)
  putStrLn (show huge)
  putStrLn (show (big + huge))
  putStrLn (show (huge + big))
  putStrLn (show (lim64 + 1))
  putStrLn (show $ the Integer (zero - five))
  putStrLn (show $ the Integer (eight - five))
  putStrLn (show $ the Integer (five - eight))
  putStrLn (show (the Integer (-1)))
  putStrLn (show (integer - integer))
  putStrLn (show (negative - negative))
  putStrLn (show (integer - negative))
  putStrLn (show (negative - integer))
  putStrLn (show (integer + negative))
  putStrLn (show (negative + integer))
  printLn integer1Digit
  printLn integer2Digit
  printLn integer3Digit
  printLn integer4Digit
  printLn integer5Digit
  printLn integer6Digit
  printLn integer7Digit
  printLn integer8Digit
  printLn integer9Digit
  printLn (- integer1Digit)
  printLn (- integer2Digit)
  printLn (- integer3Digit)
  printLn (- integer4Digit)
  printLn (- integer5Digit)
  printLn (- integer6Digit)
  printLn (- integer7Digit)
  printLn (- integer8Digit)
  printLn (- integer9Digit)
