module Main where

import Prelude hiding (Right, Left)
import Data.List
import Data.Maybe
import System.Environment

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Set (Set, fromList, toList, intersection)

data Direction = Up Int | Down Int | Right Int | Left Int deriving Show

type Point = (Int,Int)

type Wire = [Point]

enumFromToRev :: Int -> Int -> [Int]
enumFromToRev high low
    | high==low = [high]
    | high<low = []
    | otherwise = high:(enumFromToRev (high-1) low)

intersectWires :: Wire -> Wire -> [Point]
intersectWires wireLeft wireRight = 
    let resSet = (fromList wireLeft) `intersection` (fromList wireRight)
    in toList resSet

buildWire :: [Direction] -> Wire
buildWire (dir:dirs) = 
    let 
        createSegment :: Direction -> Point -> [Point]
        createSegment (Up amount) (x,y) = tail $ zip (repeat x) [y..y+amount]
        createSegment (Down amount) (x,y) = tail $ zip (repeat x) (enumFromToRev y (y-amount))
        createSegment (Left amount) (x,y) = tail $ zip (enumFromToRev x (x-amount)) (repeat y)
        createSegment (Right amount) (x,y) = tail $ zip [x..x+amount] (repeat y)

        buildSegments :: [Direction] -> [Point] -> [Point]
        buildSegments [] pts = pts
        buildSegments (dir:dirs) points = buildSegments dirs (points ++ (createSegment dir (last points)))
    in
        buildSegments dirs (createSegment dir (0,0))

distance :: Point -> Point -> Int
distance (x1,y1) (x2,y2) = abs(x1 - x2) + abs (y1 - y2)

type Parser = Parsec Void String

directionP :: Parser Direction
directionP = do
    dirChar <- upperChar
    num <- some digitChar
    let dist = (read num) :: Int
        dir = case dirChar of
            'U' -> Up
            'D' -> Down
            'L' -> Left
            'R' -> Right
    optional (char ',')
    return . dir $ dist

directionsP ::Parser [Direction]
directionsP =  some directionP
        
wireLength :: Wire -> Point -> Maybe Int
wireLength wire findIntersectPoint =
    let 
        lengthCount :: [Point] -> Point -> Int -> Maybe Int
        lengthCount [] _ _ = Nothing
        lengthCount (pt:pts) findPoint curCount
            | pt==findPoint = Just (curCount+1)
            | otherwise = lengthCount pts findPoint (curCount+1)
    in
        lengthCount wire findIntersectPoint 0

combinedWireLength :: Wire -> Wire -> Point -> Int
combinedWireLength leftWire rightWire pt = 
    fromJust (wireLength leftWire pt) + fromJust (wireLength rightWire pt)

input1 = "R8,U5,L5,D3\nU7,R6,D4,L4"
input2 = "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83"
input3 = "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
    
main = do
    inputText <-readFile "puzzle03_input.txt"
    let
        directions = map (fromJust . parseMaybe directionsP) $ lines inputText
        leftWire =  buildWire (head directions)
        rightWire = buildWire (last directions)
        intersection =  intersectWires leftWire rightWire
        closestDistance = minimum . map (combinedWireLength leftWire rightWire) $ intersection
    print closestDistance

 