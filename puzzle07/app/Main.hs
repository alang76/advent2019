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

data ProgramState = ProgramState {
    getInstructionPointer :: Int,
    getArray :: IOArray Int Int,
    getInputs :: [Int],
    getOutput :: Maybe Int,
    getResult :: Maybe Int,
    isTerminated :: Bool
} 

type Inputs = [Int] -- initial phase and following inputs

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

processOpcodes:: ProgramState -> IO (ProgramState)
processOpcodes programState  = do
    let 
        index = getInstructionPointer programState
        array = getArray programState
        inputs = getInputs programState
        output = getOutput programState
        
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

    case {- trace ("opCode=" ++ (show opCode)) -} opCode of
        Addition -> do
            doBinOp (+)
            processOpcodes $ ProgramState (index+4) array inputs output Nothing False
        Multiplication -> do
            doBinOp (*)
            processOpcodes $ ProgramState (index+4) array inputs output Nothing False
        Input -> do
            dest <- readParam 1
            writeArray array dest (head inputs)
            processOpcodes $ ProgramState (index+2) array (tail inputs) output Nothing False
        Output -> do
            outputVal <- readParamVal 1 (getFirstParam paramModes)
            return $ ProgramState (index+2) array inputs (Just outputVal) Nothing False
        JumpIfTrue -> do
            p1 <- readModedParam 1
            p2 <- readModedParam 2
            case p1 of 
                0 -> processOpcodes $ ProgramState (index+3) array inputs output Nothing False
                otherwise -> processOpcodes $ ProgramState p2 array inputs output Nothing False
        JumpIfFalse -> do
            p1 <- readModedParam 1
            p2 <- readModedParam 2
            case p1 of 
                0 -> processOpcodes $ ProgramState p2 array inputs output Nothing False
                otherwise -> processOpcodes $ ProgramState (index+3) array inputs output Nothing False
        LessThan -> do
            p1 <- readModedParam 1
            p2 <- readModedParam 2
            dest <- readParam 3
            case p1 < p2 of
                True -> writeArray array dest 1
                False -> writeArray array dest 0
            processOpcodes $ ProgramState (index+4) array inputs output Nothing False
        Equals -> do
            p1 <- readModedParam 1
            p2 <- readModedParam 2
            dest <- readParam 3
            case p1 == p2 of
                True -> writeArray array dest 1
                False -> writeArray array dest 0
            processOpcodes $ ProgramState (index+4) array inputs output Nothing False
        Terminate -> do
            result <- readArray array 1
            return $ {- trace ("result=" ++ (show result)) -} ProgramState index array inputs output (Just result) True

mkAmp :: [Int] -> Int -> IO ProgramState
mkAmp program phase = do
    arr <- newListArray  (0, (length program)-1) program  :: IO (IOArray Int Int)
    return $ ProgramState 0 arr [phase]  Nothing Nothing False

mkAmps :: [Int] -> Maybe Int -> [Int] -> IO [ProgramState]
mkAmps _ _ [] = return []
mkAmps program initialInput (phase:phases) = do
    amp <- mkAmp program phase
    let initialAmp = case initialInput of
                        Just input -> addInput amp input
                        Nothing -> amp
    restAmps <- mkAmps program Nothing phases
    return (initialAmp:restAmps)

addInput :: ProgramState -> Int -> ProgramState
addInput state input = {- trace ( show ("adding input " ++ show input ++ " existing inputs: " ++ show (getInputs state))) -} state { getInputs = (getInputs state) ++ [input]}

main = do
    --inputText <- readFile "puzzle07_input.txt"
    inputText <- readFile "puzzle07_input.txt"
    let 
        program = fromJust . parseMaybe programParserP $ trace ("puzzle=" ++ show inputText) $ trim inputText
        phaseCombinations = permutations [5..9]
        
        runAmps :: [ProgramState] -> IO Int
        runAmps amps = do
            let
                replace pos newVal list = take pos list ++ newVal : drop (pos+1) list

                runAmpStep :: 
                    [ProgramState] -> 
                    Int -> -- which amp to run
                    IO Int
                runAmpStep states ampToRun = do
                    newState <- processOpcodes (states!!ampToRun)
                    let output = fromJust $ getOutput newState
                        inputs = getInputs newState
                        newStates = replace ampToRun newState states
                        nextAmpToRun = case ampToRun == ((length states) -1) of
                            True -> 0
                            False -> ampToRun+1
                        nextState = (newStates !! nextAmpToRun)
                        newNextState = addInput nextState output -- chain output to input of next amp
                        newPreppedStates = replace nextAmpToRun newNextState newStates
                    case (isTerminated newState) of
                        True -> return (head inputs) -- last input of terminated program was the output of the last
                        False -> runAmpStep newPreppedStates nextAmpToRun
            finalOutput <- runAmpStep amps 0
            return finalOutput

    ampConfigurations <- mapM (mkAmps program (Just 0)) phaseCombinations
    res <- ( mapM runAmps ampConfigurations) :: IO [Int]

    putStrLn ("program result: " ++ (show (maximumBy (comparing snd) (zip phaseCombinations res))))
