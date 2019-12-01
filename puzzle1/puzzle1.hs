import System.Environment

calculateRequiredFuel :: Integer -> Integer
calculateRequiredFuel = subtract 2 . floor . (/3) . fromInteger

calculateRequiredFuelInclusive :: Integer -> Integer
calculateRequiredFuelInclusive mass
        | fuelNeeded > 0 = fuelNeeded + calculateRequiredFuelInclusive fuelNeeded
        | otherwise = 0
    where fuelNeeded = calculateRequiredFuel mass

main :: IO ()
main = do
    input <- readFile "puzzle1_input.txt"
    let 
        calcFuelAndSum mass sum = (+sum) . calculateRequiredFuelInclusive . read $ mass
        result = foldr calcFuelAndSum 0 . lines $ input
    putStrLn $ show result