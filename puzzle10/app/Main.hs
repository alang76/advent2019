module Main where

import Data.Map (Map, fromList)
import qualified Data.Map as Map

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Maybe
import Data.Ord
import Data.List
import Data.List.Split
import Debug.Trace

type Parser = Parsec Void String

data Location = Empty | Asteroid deriving (Show, Eq)

data AsteroidMap = AsteroidMap {
    width :: Int,
    height :: Int,
    locations :: Map (Int,Int) Location
}  deriving Show

data VisibilityMap = VisibilityMap {
    vWidth :: Int,
    vHeight :: Int,
    visibility :: Map (Int,Int) Bool
}

instance Show VisibilityMap where
    show vMap = concat . map (\row -> (concat row) ++ "\n") . chunksOf (vWidth vMap) . (map ((\val -> if val then "1" else "0") . snd)).  Map.toList $ (visibility vMap)


buildMap :: [[Location]] -> (AsteroidMap, VisibilityMap)
buildMap locations = 
    let 
        width = length $ head locations
        height = length locations
        visBuilder (y,_)= zip (zip [0::Int ..] (repeat y)) (take width $ repeat True)
        astBuilder (y, row) = zip (zip [0::Int ..] (repeat y)) row
        astCoords = map astBuilder (zip [0::Int ..] locations)
        visCoords = map visBuilder (zip [0::Int ..] locations)
    in
        ( AsteroidMap   width height (fromList . concat $ astCoords), 
          VisibilityMap width height (fromList . concat $ visCoords) )


blockLocation :: (Int, Int) -> VisibilityMap -> VisibilityMap
blockLocation coord visMap = visMap {visibility = Map.insert coord False (visibility visMap)}

isOnMap :: (Int,Int) -> VisibilityMap -> Bool
isOnMap coord visMap
    | x>=0 && x < vWidth visMap && y>=0 && y<vHeight visMap = True
    | otherwise = False
    where 
        (x,y) = (fst coord, snd coord)

blockRay :: Bool -> (Int, Int) -> Int -> Int -> VisibilityMap -> VisibilityMap
blockRay blockStart start@(x,y) ratio_x ratio_y visMap =
    let
        blockedMap = trace ("vismap pre-block, (x,y) = " ++ show start ++ " -> \n" ++ show visMap) $ if blockStart then visMap else blockLocation start visMap
    in
        case isOnMap start visMap of
            True -> trace ("vismap post-block, (x,y) = " ++ show start ++ " -> \n" ++ show blockedMap) $ blockRay True (x+ratio_x, y+ratio_y) ratio_x ratio_y blockedMap 
            False -> visMap

updateVisibilityAsteroid :: (Int, Int) -> (Int,Int) -> VisibilityMap -> VisibilityMap
updateVisibilityAsteroid pov lookAt visMap = 
    let


        xdiff = fst pov - fst lookAt
        ydiff = snd pov - snd lookAt
        divisor = gcd xdiff ydiff
        ratio_x = xdiff `div` ({- trace ("gcd for xdiff,ydiff " ++ show xdiff ++ ", " ++ show ydiff ++ " is " ++ show divisor) -} divisor)
        ratio_y = ydiff `div` divisor
    in
        blockRay False lookAt ratio_x ratio_y visMap

updateVisibilityMap :: (Int, Int) -> [(Int, Int)] -> VisibilityMap -> VisibilityMap
updateVisibilityMap _ [] visMap = visMap
updateVisibilityMap pov (loc:astlocs) visMap = updateVisibilityMap pov astlocs (updateVisibilityAsteroid pov loc visMap)

getAsteroidCoords :: AsteroidMap -> [(Int,Int)]
getAsteroidCoords astMap = map fst . filter ((==) Asteroid . snd) . Map.toList $ locations astMap

getLocation :: Char -> Location
getLocation '.' = Empty
getLocation '#' = Asteroid

countVisibleRoids :: VisibilityMap -> [(Int, Int)] -> (Int, Int) -> Int
countVisibleRoids visMap roids currentRoid = 
    let 
        newVisMap = trace ("calculating visibility for roid: " ++ show currentRoid) updateVisibilityMap currentRoid (roids \\ [currentRoid]) visMap
    in 
        trace ("newvismap roid=(" ++ show currentRoid ++ ") = \n" ++ show newVisMap) $  foldr (\coord@(x,y) cnt -> cnt + (if fromJust $ Map.lookup coord (visibility newVisMap) then 1 else 0)) 0 roids

mapRowParserP :: Parser [Location]
mapRowParserP = do
    locationChars <- some (char '.' <|> char '#')
    optional eol
    return $ (map getLocation locationChars)
    

mapParserP :: Parser [[Location]]
mapParserP = some $ do
    row <- mapRowParserP
    return row

main :: IO ()
main = do
    inputFile <- readFile "test1.txt"
    let
        (asteroidMap, visibilityMap) = buildMap . fromJust . parseMaybe mapParserP $ inputFile
        asteroidCoords = trace ("ast coords = " ++ show (getAsteroidCoords asteroidMap)) getAsteroidCoords asteroidMap
        asteroidsVisCount = map (\coord -> (countVisibleRoids visibilityMap asteroidCoords coord, coord)) asteroidCoords 
        bestRoid = maximumBy (comparing fst) asteroidsVisCount
    print asteroidsVisCount
    print "Best roid is:"
    putStrLn (show bestRoid)
