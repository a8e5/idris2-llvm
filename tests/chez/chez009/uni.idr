import Data.String

-- examples taken from https://hsivonen.fi/string-length/

foo : String
foo = "ällo"

clef : String
clef = "Clef: 𝄞"

ällo : Int
ällo = 42

facepalm : String
facepalm = "🤦🏼‍♂️"

family : String
family = "👨‍👨‍👧‍👧"

strId : String -> String
strId s = case strM s of
               StrNil => ""
               StrCons h t => strCons h t

main : IO ()
main = do printLn ällo
          putStrLn foo
          putStrLn $ strId foo
          printLn $ length foo
          putStrLn clef
          putStrLn $ cast $ assert_total $ strIndex clef 6
          printLn $ length clef
          putStrLn facepalm
          printLn $ length facepalm
          putStrLn family
          printLn $ length family
          putStrLn "Snowman: ☃️"
          putStrLn "Wikipedia: $¢ह€한𐍈"
