module Main

square : Int -> Int
square x = x * x

myMap : List Int -> List Int
myMap = map (*5) 

quadro : Int -> Int
quadro = square . square

eq : Int -> Int -> Bool
eq x y = x == y

main : IO ()
main = putStrLn "Hello, World!"