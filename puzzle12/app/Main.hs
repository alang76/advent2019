module Main where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Maybe
import Data.List

type Parser = Parsec Void String

type Vector = (Integer,Integer, Integer)

data Moon  = Moon {
    position :: Vector,
    velocity :: Vector
} deriving Eq

instance Show Moon where
    show moon = (show (position moon) ++ ", " ++ show (velocity moon) ++ "\n")

intParserP :: Parser Integer
intParserP = do
   negative <- optional (char '-') 
   let negFunc = case negative of
           Just _ -> negate
           _ -> id
   number <- some digitChar
   return . negFunc $ (read number :: Integer)

coordParser :: Parser Integer
coordParser = do
    _ <- lowerChar
    _ <- char '='
    num <- intParserP
    return num

moonParserP :: Parser [Moon]
moonParserP = some $ do
    _ <- char '<'
    x <- coordParser
    _ <- chunk ", "
    y <- coordParser
    _ <- chunk ", "
    z <- coordParser
    _ <- char '>'
    _ <- optional eol
    return $ Moon (x,y,z) (0,0,0)

getAxis :: Vector -> Char -> Integer
getAxis (x,_,_) 'x' = x
getAxis (_,y,_) 'y' = y
getAxis (_,_,z) 'z' = z
getAxis _ _ = error "getAxis: invalid input"

velocityChangeAxis :: Integer -> Integer -> Integer
velocityChangeAxis left right
    | left < right = 1
    | left == right = 0
    | left > right = -1
    | otherwise = error "velocityChangeAxis: invalid input"

positionChangeAxis :: Integer -> Integer -> Integer
positionChangeAxis position_ velocity_ = position_ + velocity_

velocityChange :: Moon -> Moon -> Moon
velocityChange leftMoon rightMoon = 
    let 
        positionLeft = position leftMoon
        positionRight = position rightMoon
        velocityLeft = velocity leftMoon
        velocityNewA axis = (getAxis velocityLeft axis) + velocityChangeAxis (getAxis positionLeft axis) (getAxis positionRight axis)
        velocityNew = (velocityNewA 'x', velocityNewA 'y', velocityNewA 'z')
    in
        leftMoon {velocity=velocityNew}

positionChange :: Moon -> Moon
positionChange moon = 
    let 
        positionNewA axis = positionChangeAxis (getAxis (position moon) axis) (getAxis (velocity moon) axis)
        positionNew = (positionNewA 'x', positionNewA 'y',positionNewA 'z')
    in moon {position = positionNew}


applyGravity ::  [Moon] -> Moon -> Moon
applyGravity [] object = object
applyGravity (subject:subjects) object = applyGravity subjects (velocityChange object subject) 

updatePosition :: [Moon] -> [Moon]
updatePosition moons = map positionChange moons

getEnergy :: (Moon -> Vector) -> Moon -> Integer
getEnergy kind moon = 
        let getA axis = abs $ getAxis (kind moon) axis
        in (getA 'x') + (getA 'y') + (getA 'z')

getTotalEnergy :: Moon -> Integer
getTotalEnergy  moon = 
        (getEnergy position moon) * (getEnergy velocity moon)
            
simulationStep :: [Moon] -> Int ->  [Moon] -> IO [Moon]
simulationStep moons curStep initialCondition = do
    let
        appliedGravity = map (\moon -> applyGravity (moons \\ [moon]) moon) moons
        updatedPositions = updatePosition appliedGravity
    case updatedPositions==initialCondition of
        True -> do
            print ("initial condition found after " ++ show curStep ++ " steps.")
            return moons
        False -> do
           simulationStep updatedPositions (curStep+1) initialCondition

compareAxis :: [Moon] -> [Integer] -> [Integer]-> Char -> Bool
compareAxis newState initialPositions initialVelocities axis =
    let newPositions = map (\moon -> getAxis (position moon) axis) newState
        newVelocities = map (\moon -> getAxis (velocity moon) axis) newState
    in (initialPositions == newPositions && initialVelocities== newVelocities)

simulationStepMax :: [Moon] -> Int -> Int -> ([Integer], [Integer],[Integer]) -> ([Integer], [Integer],[Integer]) -> IO [Moon]
simulationStepMax moons curStep maxStep ip@(ipX,ipY,ipZ) iv@(ivX,ivY,ivZ) = do
    let
        appliedGravity = map (\moon -> applyGravity (moons \\ [moon]) moon) moons
        updatedPositions = updatePosition appliedGravity
        checkAxis axis ipA ivA= 
            case compareAxis moons ipA ivA axis of
                True -> print ("Axis " ++ [axis] ++ " is the same at step: " ++ show curStep )
                False -> putStr ""
    case curStep==maxStep of
        True -> do
            return moons
        False -> do
            checkAxis 'x' ipX ivX
            checkAxis 'y' ipY ivY
            checkAxis 'z' ipZ ivZ
            simulationStepMax updatedPositions (curStep+1) maxStep ip iv

main :: IO ()
main = do
    input <- readFile "puzzle12_input.txt"
    --input <- readFile "test1.txt"
    let moons = fromJust . parseMaybe moonParserP $ input
        initialPositions axis =  map (\moon -> getAxis (position moon) axis) moons
        initialVelocities axis = map (\moon -> getAxis (velocity moon) axis) moons
    newMoons <- simulationStepMax moons 0 5000000 (initialPositions 'x', initialPositions 'y', initialPositions 'z') (initialVelocities 'x', initialVelocities 'y', initialVelocities 'z')
    print newMoons
    putStr ("Total energy : ")
    print (sum . map getTotalEnergy $ newMoons) 