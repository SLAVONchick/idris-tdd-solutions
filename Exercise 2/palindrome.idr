module Main

import Exercise2

main : IO ()
main = repl "Check that a string is a palindrome:\n> " (("\"" ++) . (++ "\"\n") . show . palindrome 5)