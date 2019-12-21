module Main where

import IntComputer

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List.Split
import Control.Monad.State

data Tile = Empty | Wall | Block | Paddle | Ball deriving Eq

instance Show Tile where
    show tile = case tile of
        Empty               -> " "
        Wall                -> "#"
        Block               -> "="
        Paddle              -> "-"
        Ball                -> "*"

readTile :: Integer -> Tile
readTile 0 = Empty
readTile 1 = Wall
readTile 2 = Block
readTile 3 = Paddle
readTile 4 = Ball


data GameWorld = GameWorld {
    tiles :: Map (Int,Int) Tile,
    score :: Integer,
    width :: Int,
    height :: Int,
    ballPos :: Integer,
    paddlePos :: Integer
}

instance Show GameWorld where
    show gameWorld = 
        let coords = [(x,y)|y<-[0..((height gameWorld)-1)], x<-[0..((width gameWorld)-1)]]
            lookupTile :: (Int,Int) -> String
            lookupTile coord = case Map.lookup coord (tiles gameWorld) of
                Just tile -> show tile
                Nothing -> "!"
            scoreLine = "score: " ++ 
                (show $ score gameWorld) ++ "\n"
        in case (width gameWorld) of
            0 -> "empty"
            otherwise -> scoreLine ++ 
                (concat 
                . map (\line -> ">>" ++ line ++"<<\n") 
                . chunksOf (width gameWorld) 
                . concatMap lookupTile $ coords)

mkGameWorld = GameWorld Map.empty 0 0 0 0 0

drawObject :: [Integer] -> GameWorld -> GameWorld
drawObject [x,y,tileNum] gameWorld = 
    case x of 
        -1 -> gameWorld {score=tileNum}
        otherwise ->
            let curTile = readTile tileNum
                (xi,yi) = (fromIntegral x, fromIntegral y) 
                newPaddlePos = case curTile of
                    Paddle -> x
                    otherwise -> paddlePos gameWorld
                newBallPos = case curTile of
                    Ball -> x
                    otherwise -> ballPos gameWorld

            in
                gameWorld {
                    tiles=Map.insert (xi,yi) curTile (tiles gameWorld), 
                    width = max (xi+1) (width $ gameWorld), 
                    height = max (yi+1) (height $ gameWorld),
                    ballPos = newBallPos,
                    paddlePos = newPaddlePos}

insertCoin :: ProgramState -> IO ()
insertCoin programState = writeMemory programState 0 2 

getJoystickInput :: Integer -> Integer -> IO Integer
getJoystickInput ball paddle
    | ball < paddle = do
        --putStrLn "going left!"
        return (-1)
    | ball == paddle = do
        --putStrLn "doing nothing!"
        return 0
    | ball > paddle = do
        --putStrLn "going right!"
        return 1

updateGame :: GameWorld -> [[Integer]] -> GameWorld
updateGame = foldr drawObject

worldUpdater :: [Integer] -> State GameWorld Integer
worldUpdater outputs = do
    curState <- get
    let chunks = chunksOf 3 outputs
        newGame = updateGame curState chunks
    put newGame
    return (score newGame)


main :: IO ()
main = do
    input <- readFile "puzzle13_input.txt"
    programState <- readProgram input
    insertCoin programState
    let 
 
        gameUpdater :: GameWorld -> [Integer]-> GameWorld
        gameUpdater gameWorld outputValues = updateGame gameWorld . chunksOf 3 $ outputValues

        inputComputer :: GameWorld -> IO Integer
        inputComputer gameWorld = 
            do
                --print gameWorld
                joyStickInput <- getJoystickInput (ballPos gameWorld) (paddlePos gameWorld)
                return joyStickInput

    
    (newProgramState, newGameWorldState) <- runStateT (processOpcodes programState inputComputer gameUpdater) mkGameWorld
    print ("Final score = " ++ show (score newGameWorldState))
    --putStrLn ("Keys: " ++ (show $ Map.keys (tiles resultWorld)))
    --putStrLn ("Number of block tiles: " ++  (show . length . filter ((==)Block) . Map.elems $ (tiles resultWorld)))
    --print resultWorld

    
