{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Char (digitToInt)
import Data.Digits (unDigits)

getDigits :: String -> [Int]
getDigits = map digitToInt

stretchPattern :: [Int] -> Int -> [Int]
stretchPattern [] _ = []
stretchPattern (x:xs) amount =  ((take amount $ repeat x) ++ stretchPattern xs amount)

calcDigit :: [Int] -> [Int] -> Int
calcDigit signal pattern = 
    (abs . sum $ zipWith (*) signal pattern) `mod` 10

calcPhase :: [Int] -> [Int] -> [Int]
calcPhase inputDigits basePattern = 
    let
        signalLength = length inputDigits
        cycledPattern = take signalLength $ cycle basePattern
        stretchAmounts = [1..signalLength] 
        stretchedPatterns = map (take signalLength . drop 1 . stretchPattern cycledPattern) stretchAmounts 
    in 
        map (calcDigit inputDigits) stretchedPatterns

calcPhases :: [Int] -> [Int] -> Int -> [Int]
calcPhases inputDigits _ 0 = inputDigits
calcPhases inputDigits pattern numPhases = 
    let 
        phaseResult = calcPhase inputDigits pattern
    in
        calcPhases phaseResult pattern (numPhases-1) 

basePattern :: [Int]
basePattern = [0, 1, 0, -1]

calcPhasesFast :: [Int] -> Int -> [Int]
calcPhasesFast digits 0 = digits
calcPhasesFast digits numPhases = 
    let 
        phaseResult = calcPhaseFast digits
    in calcPhasesFast phaseResult (numPhases-1)

calcPhaseFast :: [Int] -> [Int]
calcPhaseFast input = scanr (\d s -> (d + s) `mod` 10) 0 input

main :: IO ()
main = do
    input <- readFile "puzzle16_input.txt"
    let signalDigits = getDigits input
        inputCycled = take ((length signalDigits)*10000) (cycle signalDigits)
    let
        offset = unDigits 10 $ take 7 inputCycled
    putStrLn ("Resulting digits:\n" ++ show (take 8 . drop offset $ (calcPhasesFast inputCycled 100) ))



    