module Main where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Maybe

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set, (\\), union, fromList)

type Parser = Parsec Void String

orbitMapParser2P :: Parser [(String, String)]
orbitMapParser2P = some $ do
    let identifier = upperChar <|> digitChar
    center <- some identifier
    char ')'
    orbiter <- some identifier
    optional eol
    return $ (orbiter, center)

countOrbits :: Map String String -> Int -> String -> Int
countOrbits orbitMap count name = 
    let
        target = Map.lookup name orbitMap
    in 
        case target of
            Just newName -> case newName of 
                "COM" -> count
                otherwise -> countOrbits orbitMap (count+1) newName
            Nothing -> count

getRoute :: String -> Map String String -> [String]
getRoute name orbitMap =
    let 
        target = Map.lookup name orbitMap
    in 
        case target of
            Just newName -> case newName of
                "COM" -> ["COM"]
                otherwise -> newName:(getRoute newName orbitMap)
            Nothing -> []

main :: IO ()
main = do
    mapInput <- readFile "puzzle06_input.txt"
    --mapInput <- readFile "test1.txt"
    let 
        kvs = (fromJust . parseMaybe orbitMapParser2P) mapInput
        orbitMap = Map.fromList kvs 
        countSum = sum $ map (countOrbits orbitMap 1) $ (Map.keys orbitMap)
        routeYou = fromList $ getRoute "YOU" orbitMap :: Set String
        routeSan = fromList $ getRoute "SAN" orbitMap :: Set String
        numHops = length $ ((routeYou \\ routeSan) `union` (routeSan \\ routeYou))
    print ("sum = " ++ (show countSum) )
    print ("numHops = " ++ (show numHops) )
