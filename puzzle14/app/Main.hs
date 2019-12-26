module Main where
    
import Text.Megaparsec (Parsec, optional, some, parseMaybe)
import Text.Megaparsec.Char (char, digitChar, string, upperChar, eol)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Void (Void)
import Data.Maybe (fromJust)
import Data.Ord (compare, Ordering)
import Data.List (intersperse)
import Debug.Trace

type Parser = Parsec Void String

chemicalParserP :: Parser Chemical
chemicalParserP = do
    amountDigits <- some digitChar
    char ' '
    name <- some upperChar
    optional (string ", ")
    return $ Chemical name (read amountDigits)

reactionsParserP :: Parser [Reaction]
reactionsParserP = some $ do
    inputChemicals <- some chemicalParserP
    string " => "
    outputChemical <- chemicalParserP
    optional eol
    return $ Reaction inputChemicals outputChemical 0

type NeededMap = Map String NeededChemical

type Amount = Int
type ConsumerId = Int

data NeededChemical = NeededChemical {
    multiplier :: Int, -- how often the reaction needs to be executed
    consumers :: Map ConsumerId Amount,
    reaction :: Reaction -- reaction that produces the chemical
}

instance Show NeededChemical where
    show (NeededChemical multi consumers reaction) = 
        show multi ++ " x " ++ show reaction ++ ", " ++ "consumers = " ++ show consumers ++ "\n"

chemicalName :: NeededChemical -> String
chemicalName = name . output . reaction

totalNeeded :: NeededChemical -> Amount
totalNeeded = sum . Map.elems . consumers

totalProduced :: NeededChemical -> Amount
totalProduced neededChemical = (*) (multiplier neededChemical) . amount . output . reaction $ neededChemical 

data Chemical = Chemical {
    name :: String,
    amount :: Int
}

data Reaction = Reaction {
    inputs :: [Chemical],
    output :: Chemical,
    reactionId :: Int
} 

instance Show Chemical where
    show chemical = 
        show (amount chemical) ++ " " ++ show (name chemical)

instance Show Reaction where
    show reaction = 
        ((concat . intersperse "," . map show $ (inputs reaction)) ++ " => " ++ show (output reaction))

buildNeededMap :: [Reaction] -> (Map String NeededChemical)
buildNeededMap reactions = 
    let 
        addReaction :: Reaction -> Map String NeededChemical -> Map String NeededChemical
        addReaction reaction neededMap = 
            let
                neededChemical = NeededChemical 0 Map.empty reaction
            in
                Map.insert (name . output $ reaction) neededChemical neededMap
    in (foldr addReaction Map.empty reactions)

getMultiplier :: Int -> Int -> Int
getMultiplier needed produced
    | needed <= produced = 1
    | needed > produced = (\(f,s) -> if s>0 then f+1 else f) $ needed `divMod` produced


addConsumer :: Map String NeededChemical -> String -> ConsumerId -> Chemical -> Map String NeededChemical
addConsumer neededMap rawChemical consumerId requiredChemical= 
    let
        chemicalName = (name requiredChemical)
        amountNeeded = (amount requiredChemical)
        amountProduced = totalProduced neededChemical 
        neededChemical = fromJust . Map.lookup chemicalName $ neededMap
        sourceReaction = reaction neededChemical
        newAmountNeeded = totalNeeded newNeededChemical
        newMulti = getMultiplier newAmountNeeded (amount . output $ sourceReaction)
        newNeededChemical = neededChemical{
            consumers=(Map.insert consumerId amountNeeded (consumers neededChemical)),
            multiplier = newMulti}
        newNeededMap = Map.insert chemicalName newNeededChemical neededMap
    in
        case chemicalName == rawChemical of
            True -> neededMap -- stop recursing at raw material
            False -> 
                case newAmountNeeded > amountProduced of 
                    True -> -- demand outstrips production, recurse over all reactants with the new multi
                        let
                            requiredInputs = map (\c -> c{amount=(amount c)*newMulti}) (inputs sourceReaction)
                            
                        in 
                            foldr (\reqInput ndMp -> addConsumer ndMp rawChemical (reactionId sourceReaction) reqInput) newNeededMap requiredInputs
                    False ->
                        newNeededMap -- current production satisfies demand, no need to recurse

binarySearch :: Int -> Int -> Int -> (Int -> Int) -> Int -> Int
binarySearch lowerBound upperBound guess calc match = 
    let
        guessedAnswer = calc guess
    in
        case compare (calc guess) match of
            LT -> if (((upperBound-guess) `div` 2) > 0) then binarySearch guess upperBound (guess + ((upperBound-guess) `div` 2)) calc match else guess
            EQ -> guess
            GT -> if (((guess-lowerBound) `div` 2) > 0) then binarySearch lowerBound guess (guess - ((guess-lowerBound) `div` 2)) calc match else (guess-1)

getOreAmount ::  Map String NeededChemical -> String -> Int
getOreAmount neededMap rawMaterial = 
    let
        oreFilter = (==) rawMaterial . name . head . inputs . reaction 
        oreAmounts neededChemical = sum . map (\i -> (amount i)*(multiplier neededChemical)) . inputs . reaction $ neededChemical
    in
        sum . map oreAmounts . filter oreFilter $ Map.elems neededMap

addReactionIds :: [Reaction] -> [Int]-> [Reaction]
addReactionIds reactions ids = map (\(r,rid) -> r{reactionId=rid}) (zip reactions ids)

main :: IO ()
main = do
    input <- readFile "puzzle14_input.txt"
    let rawMaterial = "ORE"
        endProduct = "FUEL"
        reactions = (fromJust . parseMaybe reactionsParserP) input
        neededMap = buildNeededMap (addReactionIds reactions [0..])
        fuelChem = output . reaction . fromJust . Map.lookup endProduct $ neededMap
        resultMap = addConsumer neededMap rawMaterial 999 fuelChem
        oreAmount = getOreAmount resultMap rawMaterial
        calcNumFuel numFuel = 
            let 
                fuelChems = fuelChem{amount = numFuel}
                resultMap = addConsumer neededMap rawMaterial 999 fuelChems
            in 
                getOreAmount resultMap rawMaterial
    let
        totalOres = 1000000000000
        lowerBound = totalOres `div` oreAmount
        upperBound = totalOres
        finalAnswer = binarySearch lowerBound totalOres ((upperBound - lowerBound) `div` 2) calcNumFuel totalOres
    putStrLn ("final answer = " ++ show finalAnswer)



