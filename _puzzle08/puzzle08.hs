
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Data.List

-- | Standard build function.
build :: (forall b. (a -> b -> b) -> b -> b) -> [a]
build g = g (:) []

chunk :: [a] -> Int -> [[a]]
chunk list = 
    let chunkStep :: [a] -> [[a]] -> -[[a]]
        chunkStep (a:as) (c:cs)

main :: IO ()
main = do
    input <- readFile "puzzle08_input.txt"
    let pixels = map (read . (:[])) . T.unpack . T.strip . T.pack $ input :: [Int]
    print pixels