{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.ST
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (strip, pack, unpack)
import Data.Void
import Data.List
import Data.Maybe
import Data.Array.IO
import Data.Ord

import Debug.Trace

type Parser = Parsec Void String

data OpCode = 
     Addition 
   | Multiplication 
   | Input 
   | Output 
   | Terminate 
   | JumpIfTrue
   | JumpIfFalse
   | LessThan
   | Equals
   deriving Show

data ParamMode = PositionMode | ImmediateMode deriving Show

data ParamModes = ParamModes {
   getFirstParam :: ParamMode,
   getSecondParam :: ParamMode,
   getThirdParam :: ParamMode
} deriving Show

trim :: String -> String
trim = unpack . strip . pack

intParserP :: Parser Int
intParserP = do
   negative <- optional (char '-') 
   let negFunc = case negative of
           Just _ -> negate
           otherwise -> id
   number <- some digitChar
   return . negFunc $ (read number :: Int)

programParserP :: Parser [Int]
programParserP = some (do
   number <- intParserP
   optional (char ',')
   return number)
   

opCodeParserP :: Parser OpCode
opCodeParserP = do
   digits <- some digitChar
   let opCode = case (read digits :: Int) of
           1 -> Addition
           2 -> Multiplication
           3 -> Input
           4 -> Output
           5 -> JumpIfTrue
           6 -> JumpIfFalse
           7 -> LessThan
           8 -> Equals
           99 -> Terminate
   return opCode

paramModeParserP :: Parser ParamModes
paramModeParserP = do
       thirdMode <-  digitChar
       secondMode <- digitChar
       firstMode <- digitChar
       let getMode :: Char -> ParamMode
           getMode '0' = PositionMode
           getMode '1' = ImmediateMode
           paramModes = 
               ParamModes (getMode firstMode)
                          (getMode secondMode)
                          (getMode thirdMode)
       return $ paramModes

getOpCodeAndParamModes :: Int -> (OpCode, ParamModes)
getOpCodeAndParamModes i = 
   let 
       digits = show i
       (paramModesStr, opCodeStr) = splitAt (subtract 2 . length $ digits) digits
       paramModesStrFull = (take (3- (length paramModesStr)) $ repeat '0') ++ paramModesStr
   in
       (fromJust . parseMaybe opCodeParserP $ opCodeStr, fromJust . parseMaybe paramModeParserP $ paramModesStrFull)

type InOut = ([Int], Maybe Int)

processOpcodes:: Int -> IOArray Int Int -> InOut -> IO (Int, InOut)
processOpcodes index array inOut@(phases,resOutput) = do
   instruction <- readArray array index
   let (opCode,paramModes) = getOpCodeAndParamModes instruction
       
       readParam :: Int -> IO Int
       readParam offset = do
           param <- readArray array (index+offset)
           return $  param

       readParamVal :: Int -> ParamMode -> IO Int
       readParamVal offset paramMode = do
               param <- readParam offset
               paramVal <- case paramMode of
                       PositionMode -> readArray array param
                       ImmediateMode -> pure param
               return $ paramVal

       readModedParam :: Int -> IO Int
       readModedParam idx = readParamVal idx (getParam idx paramModes)
           where 
               getParam idx = case idx of
                   1 -> getFirstParam
                   2 -> getSecondParam
                   3 -> getThirdParam

       doBinOp :: (Int -> Int -> Int) -> IO ()
       doBinOp op = do
           left <- readModedParam 1
           right <- readModedParam 2
           dest <- readParam 3
           writeArray array dest (left `op` right)

   case opCode of
       Addition -> do
           doBinOp (+)
           processOpcodes (index+4) array inOut
       Multiplication -> do
           doBinOp (*)
           processOpcodes (index+4) array inOut
       Input -> do
           dest <- readParam 1
           putStrLn "input:"
           --input <- getLine
           writeArray array dest (head phases)
           processOpcodes (index+2) array (tail phases, resOutput)
       Output -> do
           outputVal <- readParamVal 1 (getFirstParam paramModes)
           putStrLn ("output:" ++ show outputVal)
           processOpcodes (index+2) array (phases, Just outputVal)
       JumpIfTrue -> do
           p1 <- readModedParam 1
           p2 <- readModedParam 2
           case p1 of 
               0 -> processOpcodes (index+3) array inOut
               otherwise -> processOpcodes p2 array inOut
       JumpIfFalse -> do
           p1 <- readModedParam 1
           p2 <- readModedParam 2
           case p1 of 
               0 -> processOpcodes p2 array inOut
               otherwise -> processOpcodes (index+3) array inOut
       LessThan -> do
           p1 <- readModedParam 1
           p2 <- readModedParam 2
           dest <- readParam 3
           case p1 < p2 of
               True -> writeArray array dest 1
               False -> writeArray array dest 0
           processOpcodes (index+4) array inOut
       Equals -> do
           p1 <- readModedParam 1
           p2 <- readModedParam 2
           dest <- readParam 3
           case p1 == p2 of
               True -> writeArray array dest 1
               False -> writeArray array dest 0
           processOpcodes (index+4) array inOut
       Terminate -> do
           result <- readArray array 1
           return (result, inOut)
   

runProgram :: [Int] -> InOut -> IO (Int, InOut)
runProgram program inout = do
   arr <- newListArray  (0, (length program)-1) program  :: IO (IOArray Int Int)
   res <- processOpcodes 0 arr inout
   return res



main = do
   inputText <-readFile "puzzle07_input.txt"
   let 
        program = fromJust . parseMaybe programParserP $ trace ("puzzle=" ++ show inputText) $ trim inputText
        phaseCombinations = permutations [0..4]
        
        runCombo :: [Int] -> Int -> IO (Int)
        runCombo [] lastProgramOutput = pure lastProgramOutput
        runCombo (phaseInput:phaseInputs) lastProgramOutput = do
            (_,(_,output)) <- runProgram program ([phaseInput,lastProgramOutput], Nothing)
            runCombo phaseInputs (fromJust output)
        
        runCombos :: [[Int]] -> [([Int], Int)] -> IO [([Int],Int)]
        runCombos [] comboResults = pure comboResults
        runCombos (combo:combos) comboResults = do
            comboRes <- runCombo combo 0
            runCombos combos ((combo, comboRes):comboResults)

   comboResults <- runCombos phaseCombinations []


   putStrLn ("program result: " ++ (show (maximumBy (comparing snd) comboResults)))
