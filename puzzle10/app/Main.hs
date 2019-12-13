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
    show vMap =
        concat . map (\row -> concat row ++ "\n") . chunksOf (vWidth vMap) . map showCoord $ [(x,y) | y<-[0..4], x<-[0..4]]
            where showCoord (x,y) = case fromJust $ Map.lookup (x,y) (visibility vMap) of
                                False -> "0"
                                True  -> "1"


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
blockRay ignoreCurrent start@(x,y) ratio_x ratio_y visMap =
    let
        blockedMap = if ignoreCurrent then visMap else blockLocation start visMap
    in
        case isOnMap start visMap of
            True -> blockRay False (x+ratio_x, y+ratio_y) ratio_x ratio_y blockedMap 
            False -> visMap

updateVisibilityAsteroid :: (Int, Int) -> (Int,Int) -> VisibilityMap -> VisibilityMap
updateVisibilityAsteroid pov lookAt visMap = 
    let
        xdiff =  fst lookAt - fst pov
        ydiff =  snd lookAt -  snd pov
        divisor = gcd xdiff ydiff
        ratio_x = xdiff `div` divisor
        ratio_y = ydiff `div` divisor
    in
        blockRay True lookAt ratio_x ratio_y visMap

updateVisibilityMap :: (Int, Int) -> [(Int, Int)] -> VisibilityMap -> VisibilityMap
updateVisibilityMap _ [] visMap = visMap
updateVisibilityMap pov (loc:astlocs) visMap = updateVisibilityMap pov astlocs (updateVisibilityAsteroid pov loc visMap)

getAsteroidCoords :: AsteroidMap -> [(Int,Int)]
getAsteroidCoords astMap = map fst . filter ((==) Asteroid . snd) . Map.assocs $ locations astMap

getLocation :: Char -> Location
getLocation '.' = Empty
getLocation '#' = Asteroid

countVisibleRoids :: VisibilityMap -> [(Int, Int)] -> (Int, Int) -> Int
countVisibleRoids visMap roids currentRoid = 
    let 
        newVisMap = updateVisibilityMap currentRoid (roids \\ [currentRoid]) visMap
    in 
        foldr (\coord@(x,y) cnt -> cnt + (if fromJust $ Map.lookup coord (visibility newVisMap) then 1 else 0)) 0 roids

mapRowParserP :: Parser [Location]
mapRowParserP = do
    locationChars <- some (char '.' <|> char '#')
    optional eol
    return $ (map getLocation locationChars)
    

mapParserP :: Parser [[Location]]
mapParserP = some $ do
    row <- mapRowParserP
    return row

type AngleGroup =  (Float, [(Float, (Int,Int))])
type AngleDist =  (Float, Float, (Int,Int))

calcAngle :: (Int,Int) -> (Int,Int) -> AngleDist
calcAngle pov@(xp,yp) target@(xt,yt) = 
    let (xdiff,ydiff) = (xp-xt,yp-yt)
        angle = atan2 (negate (fromIntegral xdiff)) (fromIntegral ydiff)
        distSq = xdiff^2 + ydiff^2
        anglePos = if angle<0 then ((2*pi)+angle) else angle
    in
       (anglePos, fromIntegral distSq, target)

extractFirst :: (a, b, c) -> a
extractFirst (a,_,_) = a

calcAngles :: (Int, Int) -> [(Int, Int)] -> [AngleGroup]
calcAngles laserRoid otherRoids = 
    let 
        sortDistance :: AngleGroup -> AngleGroup
        sortDistance (angle, elems)  = (angle, sortBy (comparing fst) elems)

        angleGrouper :: [AngleGroup] -> AngleGroup -> [AngleDist] -> [AngleGroup]
        angleGrouper prevGroups curGroup [] = prevGroups ++ [sortDistance curGroup]
        angleGrouper prevGroups curGroup@(curAngle, curList) ((newRoidAngle, newRoidDist, newRoidCoords):roids) 
            | curAngle == newRoidAngle = angleGrouper prevGroups (curAngle, ((newRoidDist, newRoidCoords):curList)) roids
            | otherwise = angleGrouper (prevGroups ++ [(sortDistance curGroup)]) (newRoidAngle, [(newRoidDist,newRoidCoords)]) roids

        
        ((ang,dist,coord):angleDists) = sortBy (comparing extractFirst) $ map (calcAngle laserRoid) $ otherRoids
              
    in
        angleGrouper [] (ang, [(dist,coord)]) angleDists

asteroidZapper :: AsteroidMap -> [(Int,Int)] -> (Int,Int) -> IO ()
asteroidZapper asteroidMap asteroidCoords bestRoid = do
    let
        angleGroups = calcAngles bestRoid (asteroidCoords \\ [bestRoid])

        zapper :: [AngleGroup] -> Int -> IO ()
        zapper [] zapCount = putStrLn "Done. All are zapped!"
        zapper ((curAngle, ((curDist, curCoord):others)):groups) zapCount = do
            putStrLn ("Asteroid " ++ show zapCount ++ " is vaped at: " ++ show curCoord)
            case others of 
                [] -> zapper groups (zapCount+1)
                _ -> zapper (groups ++ [(curAngle, others)]) (zapCount+1)
    zapper angleGroups 1
            
findBestRoid :: AsteroidMap -> VisibilityMap -> [(Int,Int)] -> IO (Int,(Int,Int))
findBestRoid asteroidMap visibilityMap asteroidCoords = do
    let
        asteroidsVisCount = map (\coord -> (subtract 1 $ countVisibleRoids visibilityMap asteroidCoords coord, coord)) asteroidCoords 
        bestRoid = maximumBy (comparing fst) asteroidsVisCount
    print "Best roid is:"
    putStrLn (show bestRoid)
    return bestRoid

main :: IO ()
main = do
    inputFile <- readFile "puzzle10_input.txt"
    let
        (asteroidMap, visibilityMap) = buildMap . fromJust . parseMaybe mapParserP $ inputFile
        asteroidCoords = getAsteroidCoords asteroidMap
    (bestScore, bestRoid) <- findBestRoid asteroidMap visibilityMap asteroidCoords
    asteroidZapper asteroidMap asteroidCoords bestRoid
    
