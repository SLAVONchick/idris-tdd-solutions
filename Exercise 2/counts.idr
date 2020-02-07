module Main

import Exercise2

main : IO ()
main = repl "Enter a string:\n> " ((++ "\n") . show . counts)
