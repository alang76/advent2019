module Main where

import Prelude hiding (Left, Right)
import IntComputer (ProgramState, processOpcodes, writeMemory, readProgram)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Char (ord, chr)
import Control.Monad.State (runStateT)

type Coord = (Int,Int)

getX :: Coord -> Int
getX = fst

getY :: Coord -> Int
getY = snd

data Direction = Up | Right | Down | Left deriving Eq

nextCoord :: Coord -> Direction -> Coord
nextCoord (x,y) Up = (x,y-1)
nextCoord (x,y) Right = (x+1,y)
nextCoord (x,y) Down = (x,y+1)
nextCoord (x,y) Left = (x-1,y)

turnLeft :: Direction -> Direction
turnLeft Up = Left
turnLeft Right = Up
turnLeft Down = Right
turnLeft Left = Down

turnRight :: Direction -> Direction
turnRight Up = Right
turnRight Right = Down
turnRight Down = Left
turnRight Left = Up

data Tile = OpenSpace | Scaffold | Intersection | DroidC Droid deriving Eq

instance Show Tile where
    show OpenSpace = "."
    show Scaffold = "#"
    show Intersection = "O"
    show (DroidC c) = show c

data Droid = Droid {
    position :: Coord,
    direction :: Direction
} deriving Eq

mkDroid :: Droid
mkDroid = Droid (-1,-1) Up

instance Show Droid where
    show droid' = case direction droid' of
        Up -> "^"
        Right -> ">"
        Down -> "v"
        Left -> "<"

data ProcessStage = Initial | RequestInput | ContVideo

data World = World {
    worldMap :: Map Coord Tile,
    width :: Int,
    height :: Int,
    droid :: Droid,
    moveInputs :: [[Integer]],
    processStage :: ProcessStage,
    outputCoord :: Coord
}

instance Eq World where
    w1 == w2 = (moveInputs w1 == moveInputs w2)

superImposeDroid :: Map Coord Tile -> Droid -> Map Coord Tile
superImposeDroid worldMap' droid' = Map.insert (position droid') (DroidC droid') worldMap'

moveInputs' :: [[Integer]]
moveInputs' = 
    let 
        -- todo: encode automatically
        mainFunc = "B,C,C,A,B,C,A,B,C,A"
        aFunc = "L,6,L,10,L,10,R,6"
        bFunc = "L,6,R,12,L,4,L,6"
        cFunc = "R,6,L,6,R,12"
        feed = "n"
        addNewLine line = line ++ [ord '\n']
    in
        map (map toInteger . addNewLine . map ord) [mainFunc,aFunc,bFunc,cFunc,feed]

data DroidMove = TurnLeft | TurnRight | MoveForward Int deriving Eq

instance Show DroidMove where
    show TurnLeft = "L"
    show TurnRight = "R"
    show (MoveForward num) = show num

instance Show World where
    show world = 
        let world' = worldMap world 
            coordMap = map (\y->[(x,y)|x<-[0..((width world)-1)]]) [0..((height world-1))]
        in concatMap (\xs -> concatMap (\coord -> show . fromJust $ Map.lookup coord world') xs ++ "\n") $ coordMap

lookupTile :: World -> Coord -> Maybe Tile
lookupTile = flip Map.lookup . worldMap

mkWorld :: World
mkWorld = World Map.empty 0 0 mkDroid moveInputs' Initial (0,0)

setWorldMeta :: World -> World
setWorldMeta world = 
    let coords = Map.keys (worldMap world)
        widthM = maximum $ map fst coords
        heightM = maximum $ map snd coords
    in
        world {width=widthM, height=heightM}

updateWorld :: World -> Integer -> Maybe World
updateWorld inputWorld outputValue = setWorld outputValue inputWorld
    where
        setWorld :: Integer -> World -> Maybe World
        setWorld asciiCode world = 
            let coord = outputCoord world 
                maybeTile = getTile asciiCode
                newDroid = updateDroid (droid world) coord asciiCode
                newDroidWorld = world{droid=newDroid}
                (newCoord, newWorld) = 
                    (updateCoord asciiCode coord, 
                    case maybeTile of
                        Just tile -> setTile coord tile newDroidWorld
                        Nothing -> newDroidWorld)
            in if (asciiCode==10 && ((getX coord) == 0))
                then Nothing -- newline after newline indicates end of world updates 
                else Just newWorld{outputCoord=newCoord}

        setTile :: Coord -> Tile -> World -> World
        setTile coord tile world = world{worldMap=Map.insert coord tile (worldMap world)}

        updateCoord :: Integer -> Coord -> Coord
        updateCoord 10 (_,y)= (0,y+1)
        updateCoord _ (x,y)= (x+1,y)

        updateDroid :: Droid -> Coord -> Integer -> Droid
        updateDroid droid' newCoord droidUpdate = 
            let 
                setDroidDirection dir = droid' {position = newCoord, direction = dir}
            in
                case droidUpdate of
                    94 -> setDroidDirection Up
                    62 -> setDroidDirection Right
                    118 ->setDroidDirection Down
                    60 -> setDroidDirection Left
                    _ -> droid'
                

        getTile :: Integer -> Maybe Tile
        getTile 35 = Just Scaffold
        getTile 46 = Just OpenSpace
        getTile _ = Nothing



inputGeneratorPart1 :: World -> IO (World, Maybe Integer)
inputGeneratorPart1 world = pure (world, Nothing)



outputProcessor :: World -> Integer -> IO World
outputProcessor world output =
    case (processStage world) of 
        Initial ->  
            case updateWorld world output of
                Nothing -> pure . setWorldMeta $ world{processStage=RequestInput}
                Just world' -> pure world'
        _ -> do
            case output >= 255 of
                True -> putStrLn ("Final result: " ++ show output)
                False -> putChar . chr . fromInteger $ output
            return world

getFirst :: [[a]] -> Maybe (a,[[a]])
getFirst ([]:lists) = getFirst lists
getFirst (list:lists) = Just (head list, (tail list) : lists)
getFirst _ = Nothing

inputGeneratorManual :: World -> IO (World, Maybe Integer)
inputGeneratorManual world = do
    input <- getLine
    return (world, Just (read input))

inputGeneratorPart2 :: World -> IO (World, Maybe Integer)
inputGeneratorPart2 world = 
    let
        first = getFirst (moveInputs world)
    in
        case (processStage world) of
            Initial -> error "Not expecting input in initial phase"
            RequestInput -> 
                case first of 
                    Just (first', rest) -> pure (world{moveInputs=rest}, Just first')
                    Nothing -> pure (world, Nothing)
            ContVideo -> error "Not expecting input in video phase"

printAlignmentParameterSum :: World -> IO ()
printAlignmentParameterSum world = do
    let
        coords = [(x,y)|x<-[0..(width world)],y<-[0..(height world)]]
        intersectionCoords = filter isIntersection coords
        alignmentParam (x,y) = x*y
        isIntersection (x,y) = 
            (lookupTile world (x,y)==Just Scaffold) --point itself must be on scaffold
            && ((flip (>=)) 3 . length . filter (==(Just Scaffold)) $ 
                map (lookupTile world) [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]) --at least 3 neighbours on the scaffold
    print . sum . map alignmentParam $ intersectionCoords

tryMoveForward :: World -> (World, Maybe DroidMove)
tryMoveForward = tryMoveForward' 0
    where
        tryMoveForward' :: Int -> World -> (World, Maybe DroidMove)
        tryMoveForward' num world  = 
            let
                droid' = droid world
                droidPos = position droid'
                droidDir = direction droid'
                
                nextCoord' = nextCoord droidPos droidDir
                tileAtNextCoord = lookupTile world nextCoord' 
            in 
                case tileAtNextCoord of
                    Just Scaffold -> tryMoveForward' (num+1) world{droid=droid'{position=nextCoord'}} 
                    _ -> case num of
                        0 -> (world, Nothing)
                        n -> (world, Just (MoveForward n))

tryTurn :: World -> (World, Maybe DroidMove)
tryTurn world = 
    let
        droid' = droid world
        droidPos = position droid'
        droidDir = direction droid'

        leftDirection = turnLeft droidDir
        rightDirection = turnRight droidDir
        turnLeftCoord = nextCoord droidPos leftDirection
        turnRightCoord = nextCoord droidPos rightDirection
        leftTile = lookupTile world turnLeftCoord
        rightTile = lookupTile world turnRightCoord
    in
        case leftTile of
            Just Scaffold -> (world{droid=droid'{direction=leftDirection}}, Just TurnLeft)
            _ -> case rightTile of 
                Just Scaffold -> (world{droid=droid'{direction=rightDirection}}, Just TurnRight)
                _ -> (world,Nothing)
                
getDroidMove :: World -> (World, Maybe DroidMove)
getDroidMove world =
        case tryMoveForward world of
            mf@(_, Just _) -> mf
            (_, Nothing) -> 
                let t@(_, turnMove) = tryTurn world
                in case turnMove of
                    Just _ -> t
                    Nothing -> (world, Nothing)


getDroidMoves :: World -> [DroidMove]
getDroidMoves world = 
    let
        (newWorld, maybeMove) = getDroidMove world
    in 
        case maybeMove of 
            Just mv -> mv : (getDroidMoves newWorld)
            Nothing -> []

part1 :: ProgramState -> IO ()
part1 programState = do
    (_,finalWorld) <- runStateT (processOpcodes programState inputGeneratorPart1 outputProcessor) mkWorld
    print finalWorld
    printAlignmentParameterSum finalWorld

part2 :: ProgramState -> IO ()
part2 programState = do
    writeMemory programState 0 2 -- wake up robot 
    (_,finalWorld) <- runStateT (processOpcodes programState inputGeneratorPart2 outputProcessor) mkWorld
    let
        droidMoves = getDroidMoves finalWorld
    putStrLn . show $ (finalWorld{worldMap = superImposeDroid (worldMap finalWorld) (droid finalWorld)})
    putStrLn . show $ droidMoves
    putStrLn "done"

main :: IO ()
main = do
    inputProgramText <- readFile "puzzle17_input.txt"
    programState <- readProgram inputProgramText    
    --part1 programState
    part2 programState


    
