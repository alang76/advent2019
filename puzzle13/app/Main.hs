module Main where

import IntComputer

main :: IO ()
main = do
    input <- readFile "puzzle13_input.txt"
    programState <- readProgram input
    outputState <- processOpcodes programState
    print outputState
