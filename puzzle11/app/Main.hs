{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where


import Prelude hiding (Right, Left)

import Data.List
import Data.Array.IO
import Data.Ord hiding (Down)
import Data.List.Split
import IntComputer

import Debug.Trace

turnRight :: Direction -> Direction
turnRight Up = Right
turnRight Right = Down
turnRight Down = Left
turnRight Left = Up

turnLeft :: Direction -> Direction
turnLeft Up = Left
turnLeft Left = Down
turnLeft Down = Right
turnLeft Right = Up

data Color = Black | White deriving Show

type Coord = (Int,Int)

data Direction = Up | Right | Down | Left deriving Show

data Robot = Robot {
    position :: Coord,
    direction :: Direction
} deriving Show

data Panel = Panel {
    color :: Color,
    isPainted :: Bool
} deriving Show

data Hull = Hull {
    width :: Int,
    height :: Int,
    panels :: IOArray Int Panel
}

data World = World {
    hull :: Hull,
    robot :: Robot,
    programState :: ProgramState
}

getIndex :: Hull -> Coord -> Int
getIndex hull (x,y) = (y*(width hull)) + x

mkHull :: Int -> Int -> IO Hull
mkHull width height = do
    arr <- newArray (0,width*height) (Panel Black False) :: IO (IOArray Int Panel)
    return $ Hull width height arr

mkWorld :: ProgramState -> IO World
mkWorld programState = do
    hull <- mkHull 100 100
    robot <- pure $ Robot (50,50) Up
    let world' = World hull robot programState
    paint White world'

paint :: Color -> World -> IO (World)
paint color world = do
    let curPanels = panels . hull $ world
        hull' = hull world
        coord = position .  robot $ world
    writeArray curPanels (getIndex hull' coord) (Panel color True)
    return $ world {hull = hull'{panels = curPanels}}

paintHull :: Integer -> World -> IO (World)
paintHull colorIndex world = 
    case colorIndex of
        0 -> paint Black world
        1 -> paint White world

move :: Coord -> Direction -> Coord
move (x,y) Up = (x,y-1)
move (x,y) Right = (x+1,y)
move (x,y) Down = (x,y+1)
move (x,y) Left = (x-1,y)

turnAndMoveRobot :: Integer ->  World -> IO World
turnAndMoveRobot directionIndex world = do
    let 
        robot' = robot world
        turnedRobot = case directionIndex of
            0 -> robot' {direction = turnLeft . direction $ robot'}
            1 -> robot' {direction = turnRight . direction $ robot'}
        movedRobot = turnedRobot {position = move (position turnedRobot) (direction turnedRobot)}
    return $ world {robot=movedRobot}

senseColor :: World -> IO Color
senseColor world =  do
    let arr = panels . hull $ world
        coord =  position . robot $ world
    panel' <- readArray arr (getIndex (hull world) coord)
    return $ color panel'

processOutputs :: World -> IO World
processOutputs world = do
    let 
        outputs = outputValues . programState $ world 
    case outputs of
        [] -> return world
        o1:o2:rest -> do
            paintedWorld <- paintHull o1 $ world
            turnedAndMovedWorld <-turnAndMoveRobot o2 paintedWorld
            processOutputs turnedAndMovedWorld {programState = (programState world) {outputValues = rest}}

paintHullStep :: World -> IO World
paintHullStep world =  do
    curColor <- senseColor world
    let pState = programState world
        robotInput = case curColor of 
            Black -> 0
            White -> 1
        newPState = pState {inputValues = (inputValues pState) ++ [robotInput]}
    resultState <- processOpcodes newPState
    case terminated resultState of 
        True -> return $ world {programState=resultState}
        False -> processOutputs $ world {programState=resultState}

paintWorld :: World -> IO (World)
paintWorld world = do
    resultWorld <- paintHullStep world
    case terminated . programState $ resultWorld of 
        True -> do
            putStrLn "Finished"
            return resultWorld
        False -> do
            newWorld <- paintWorld resultWorld
            return (newWorld)

printWorld :: World -> Int -> Int -> IO ()
printWorld world idx numPanels = do
    putStr $ case (snd $ idx `divMod` (width . hull $ world)) of
        0 ->  "\n"
        otherWise -> ""
    panel <- readArray (panels . hull $ world ) idx
    let newNumPanels = if (isPainted panel) then numPanels+1 else numPanels
    case idx==getIndex (hull world) (position . robot $ world) of
        True -> putStr $ case (direction . robot $ world) of
            Up -> "^"
            Right -> ">"
            Down -> "v"
            Left -> "<"
        otherwise -> do
            putStr $ case color panel of
                White -> "#"
                Black -> "."
    case idx >= ((width . hull $ world) * (height . hull $ world)) of
        True -> do
            putStrLn ("num panels painted = " ++ show numPanels)
            return ()
        False -> do
            printWorld world (idx+1) newNumPanels


main = do
    inputText <- readFile "puzzle11_input.txt"
    let 
        program = readProgram inputText

    programState' <- initializeProgram program
    world <- mkWorld programState'
    newWorld <- paintWorld world
    putStrLn ("program result: " ++ show (resultValue . programState $ newWorld))
    printWorld newWorld 0 0
    