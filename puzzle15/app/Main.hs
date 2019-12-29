{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}


module Main where

import IntComputer
import Safe
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Query.BFS
import Data.Graph.Inductive.PatriciaTree
import Data.Map (Map)
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import Control.Monad.State
import Debug.Trace

data Tile = Wall | Open | Oxygen deriving (Show,Eq) 
data Direction = North | East | South | West deriving (Show,Eq)
type Coord = (Int,Int)
type NodeLabel = (Coord, Tile)
type MapNode = LNode NodeLabel
type IsOrigin = Bool
type EdgeLabel = (Direction, IsOrigin)
type MapEdge = LEdge EdgeLabel
type Ship = Gr NodeLabel EdgeLabel
type MapNodeMap = Map (Int,Int) MapNode

getNodeLabel :: MapNode -> NodeLabel
getNodeLabel = snd

getNode :: MapNode -> Node
getNode = fst

data ExploreState = ExploreState {
    ship :: Ship,
    node :: MapNode,
    nodeMap :: MapNodeMap,
    exploreDirection :: Maybe Direction,
    startNode :: MapNode,
    endNode :: Maybe MapNode
}

directions = [North, East, South, West]

reverseDirection :: Direction -> Direction
reverseDirection North = South
reverseDirection East = West
reverseDirection South = North
reverseDirection West = East

getNewLocation :: Coord -> Direction -> Coord
getNewLocation (x,y) North = (x,y+1)
getNewLocation (x,y) East = (x+1,y)
getNewLocation (x,y) South = (x,y-1)
getNewLocation (x,y) West = (x-1,y)

directionToInteger :: Direction -> Integer
directionToInteger North = 1
directionToInteger East =  4
directionToInteger South =  2
directionToInteger West =  3

getTile :: Integer -> Tile
getTile 0 = Wall
getTile 1 = Open
getTile 2 = Oxygen

getExploredDirections :: Ship -> MapNode -> [Direction]
getExploredDirections ship node = 
    let
        outEdges = out ship (getNode node)
        inEdges = inn ship (getNode node)
    in
        (map (reverseDirection . fst . edgeLabel) inEdges) ++ map (fst . edgeLabel) outEdges

getExploreDirection :: Ship -> MapNode -> Maybe Direction
getExploreDirection ship node = 
    let exploredDirections = getExploredDirections ship node
        directionsToExplore =
            directions \\ exploredDirections
    in 
        case directionsToExplore of 
            [] -> Nothing
            (hd:_) -> Just hd

mkExploreState :: ExploreState
mkExploreState = 
    let 
        startShip = empty
        startNode = (head $ newNodes 1 startShip, ((0,0), Open))
        initialMap = Map.insert (0,0) startNode Map.empty
    in
        ExploreState startShip startNode initialMap (getExploreDirection startShip startNode) startNode Nothing

moveDirection :: Ship -> MapNode -> MapNode -> Maybe Direction
moveDirection curShip curNode startNode = 
    let
        inEdges = inn curShip (getNode curNode)
        originDirection = fmap reverseDirection . headMay . map fst . filter snd . map edgeLabel $ inEdges
        exploreDirection = getExploreDirection curShip curNode
    in
        case getNodeLabel curNode of
            (_, Wall) ->  originDirection
            (coord, tile) ->
                case exploreDirection of
                    Just dir -> 
                        Just dir
                    Nothing ->  case fst (getNodeLabel curNode) of
                        (0,0) ->
                            Nothing 
                        otherwise ->
                            originDirection
            
addNode :: Ship -> MapNode -> MapNodeMap -> Direction -> Tile -> (Ship, MapNode, MapNodeMap) 
addNode ship node mapNodeMap direction tile = 
    let
        (coord, _) = getNodeLabel node
        newLocation = getNewLocation coord direction
        existingNode = Map.lookup newLocation mapNodeMap
    in
        case existingNode of
            Just existingNode' -> -- node exists, create only new edge
                let 
                    edge' = (getNode node, getNode existingNode', (direction, False))
                in
                    case hasLEdge ship edge' of
                        True -> -- edge already exists, just update position
                            (ship, existingNode', mapNodeMap)
                        False -> -- create new edge
                            (insEdge edge' ship, existingNode', mapNodeMap)
            Nothing ->
                let
                    newLabel = (newLocation, tile)
                    newNode = (head $ newNodes 1 ship, newLabel)
                    newGraph = insNode newNode ship
                    newEdge = (getNode node, getNode newNode, (direction, True))
                    newNodeMap = Map.insert newLocation newNode mapNodeMap
                in 
                    (insEdge newEdge newGraph, newNode, newNodeMap)
        
inputGenerator :: ExploreState -> IO (Maybe Integer)
inputGenerator es = pure . fmap directionToInteger $  moveDirection  (ship es) ( node es) (startNode es)

outputProcessor :: ExploreState -> [Integer] -> IO ExploreState
outputProcessor exploreState [statusCode] = 
    let
        curShip = ship exploreState
        curNode = node exploreState
        sNode = startNode exploreState
        cureNodeMap = nodeMap exploreState
        exploredDirection = exploreDirection exploreState
        inEdges = inn curShip (fst curNode)
        origin = headMay . filter (snd . edgeLabel) $ inEdges
    in
        case exploredDirection of
            Just dir -> 
                do
                    let 
                        (newShip, newNode, newNodeMap) = addNode curShip curNode cureNodeMap dir (getTile statusCode)
                        getNewExploreDirection toNode = moveDirection newShip toNode sNode
                    case statusCode of
                        0 -> -- make a wall node, position stays the same
                            let newExploreDirection = getNewExploreDirection curNode
                            in  pure exploreState {ship = newShip, nodeMap = newNodeMap, exploreDirection = newExploreDirection}
                        1 -> -- make new open node and update position to it
                            let newExploreDirection =  getNewExploreDirection newNode
                            in pure exploreState {ship = newShip, node = newNode, nodeMap = newNodeMap, exploreDirection = newExploreDirection}
                        2 -> -- make new oxygen node and update position to it
                            let newExploreDirection =  getNewExploreDirection newNode
                            in pure exploreState {ship = newShip, node = newNode, endNode=Just newNode, nodeMap = newNodeMap, exploreDirection = newExploreDirection}
            Nothing -> do
                return exploreState                
outputProcessor exploreState _ = do
    return exploreState

getShortestPathLength :: ExploreState -> Int
getShortestPathLength exploreState = 
    let
        finalShip = ship exploreState
        sNode = getNode $ startNode exploreState
        eNode = getNode . fromJust $ endNode exploreState
    in
        (subtract 1) . length $ esp sNode eNode finalShip 

getOxygenFillupTime :: ExploreState -> Int
getOxygenFillupTime exploreState =
    let
        finalShip = ship exploreState
        eNode = getNode . fromJust $ endNode exploreState
    in
        (subtract 2) . maximum . map length $ bft eNode finalShip

main :: IO ()
main = do
    input <- readFile "puzzle15_input.txt"
    programState <- readProgram input
    (resultProgramState,finalExploreState) <- runStateT (processOpcodes programState inputGenerator outputProcessor) mkExploreState
    putStrLn ("Shortest path length is: " ++ (show $ getShortestPathLength finalExploreState))
    putStrLn ("Oxygen fillup time is: " ++ (show $ getOxygenFillupTime finalExploreState))