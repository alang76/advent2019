
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Data.List.Split
import Data.List
import Data.Ord
import Data.Array

width = 25
height = 6

countDigits :: Int -> [Int] -> Int
countDigits _ [] = 0
countDigits digit (l:ls)
    | digit==l = 1 + (countDigits digit ls)
    | otherwise = countDigits digit ls

arrayImage :: [[Int]] -> [Array Int Int] 
arrayImage layers = map (\layer -> listArray (0,(length layer)-1) layer)  layers

getVisiblePixel :: [Array Int Int] -> Int -> Int
getVisiblePixel (layer:layers) index = 
    let 
        pixel = layer ! index
    in 
        case pixel of 
            0 -> 0
            1 -> 1
            2 -> getVisiblePixel layers index

fillImage :: [Array Int Int] -> Int -> [Int]
fillImage layers imageSize = map (getVisiblePixel layers) [0..(imageSize-1)]

printImage :: [Int] -> Int -> IO [()]
printImage pixels width = do
    let lines = chunksOf width pixels
        printPixels :: [Int] -> String
        printPixels line = concat $ map printPixel line 

        printPixel :: Int -> String
        printPixel c = case c of
            0 -> " "
            1 -> "X"

    mapM putStrLn ((map printPixels) lines)

main :: IO [()]
main = do
    input <- readFile "puzzle08_input.txt"
    let pixels = map (read . (:[])) . T.unpack . T.strip . T.pack $ input :: [Int]
        imageSize = (width*height)
        layers = chunksOf imageSize pixels 
        arrImg = arrayImage layers
        resImage = fillImage arrImg imageSize
        {-
        num_zeros = map (countDigits 0) layers
        layersWithZeroCount = zip num_zeros layers
        (_, minLayer) = minimumBy (comparing fst) layersWithZeroCount
        numOnes = countDigits 1 minLayer
        numTwos = countDigits 2 minLayer
    print (show $ numOnes * numTwos)-}
    printImage resImage width