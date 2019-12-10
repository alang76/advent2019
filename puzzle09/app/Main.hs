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
   | JumpIfTrue
   | JumpIfFalse
   | LessThan
   | Equals
   | AdjustRelativeBase
   | Terminate 
   deriving Show

data ParamMode = PositionMode | ImmediateMode | RelativeMode deriving Show

data ParamModes = ParamModes {
   getFirstParam :: ParamMode,
   getSecondParam :: ParamMode,
   getThirdParam :: ParamMode
} deriving Show

data ProgramState = ProgramState {
    instructionPointer :: Integer,
    memory :: IOArray Integer Integer,
    inputValues :: [Integer],
    relativeBase :: Integer,
    outputValue :: Maybe Integer,
    resultValue :: Maybe Integer,
    terminated :: Bool
}

instance Show ProgramState where
    show ps = "[ip=" ++ show (instructionPointer ps) ++
              ",in=" ++ show (inputValues ps) ++
              ",rb=" ++ show (relativeBase ps) ++
              ",out=" ++ show (outputValue ps) ++
              ",res=" ++ show (resultValue ps) ++
              ",trm=" ++ show (terminated ps) ++ "]"

trim :: String -> String
trim = unpack . strip . pack

intParserP :: Parser Integer
intParserP = do
   negative <- optional (char '-') 
   let negFunc = case negative of
           Just _ -> negate
           otherwise -> id
   number <- some digitChar
   return . negFunc $ (read number :: Integer)

programParserP :: Parser [Integer]
programParserP = some (do
   number <- intParserP
   optional (char ',')
   return number)
   
opCodeParserP :: Parser OpCode
opCodeParserP = do
   digits <- some digitChar
   let opCode = case (read digits :: Integer) of
           1 -> Addition
           2 -> Multiplication
           3 -> Input
           4 -> Output
           5 -> JumpIfTrue
           6 -> JumpIfFalse
           7 -> LessThan
           8 -> Equals
           9 -> AdjustRelativeBase
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
           getMode '2' = RelativeMode
           paramModes = 
               ParamModes (getMode firstMode)
                          (getMode secondMode)
                          (getMode thirdMode)
       return $ paramModes

getOpCodeAndParamModes :: Integer -> (OpCode, ParamModes)
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
        index = instructionPointer programState
        array = memory programState

    instruction <- readArray array index

    let (opCode,paramModes) = getOpCodeAndParamModes instruction
        
        readParam :: Integer -> IO Integer
        readParam offset = do
            param <- readArray array (index+offset)
            return $  param

        readParamVal :: Integer -> ParamMode -> IO Integer
        readParamVal offset paramMode = do
                param <- readParam offset
                paramVal <- case paramMode of
                        PositionMode -> readArray array param
                        ImmediateMode -> pure param
                        RelativeMode ->  readArray array ((relativeBase programState) + param)
                return $ paramVal

        readDestVal :: Integer -> ParamMode -> IO Integer
        readDestVal offset paramMode = do
            param <- readParam offset
            let paramVal = case paramMode of
                    PositionMode -> param
                    RelativeMode -> (relativeBase programState) + param
                    ImmediateMode -> error "Cannot specify destination address in immediate mode!"
            return paramVal
            
        readModedParam :: Integer -> IO Integer
        readModedParam idx = readParamVal idx (getParam idx paramModes)

        readModedDest :: Integer -> IO Integer
        readModedDest idx = readDestVal idx (getParam idx paramModes)

        getParam :: Integer -> ParamModes -> ParamMode
        getParam idx = case idx of
            1 -> getFirstParam
            2 -> getSecondParam
            3 -> getThirdParam
        
        readDest :: Integer -> IO Integer
        readDest idx = readDestVal idx (getParam idx paramModes)

        doBinOp :: (Integer -> Integer -> Integer) -> IO ()
        doBinOp op = do
            left <- readModedParam 1
            right <- readModedParam 2
            dest <- readModedDest 3
            writeArray array dest (left `op` right)

    case opCode of
        Addition -> do
            doBinOp (+)
            processOpcodes $ programState {instructionPointer = index+4}
        Multiplication -> do
            doBinOp (*)
            processOpcodes $ programState {instructionPointer = index+4} 
        Input -> do
            dest <- readModedDest 1
            putStr ("In: ")
            inputVal <- getLine
            writeArray array dest (read inputVal)
            processOpcodes $ programState {instructionPointer = index+2, inputValues = []}
        Output -> do
            outputVal <- readModedParam 1
            putStrLn ("Out: " ++ show outputVal)
            processOpcodes $ programState {instructionPointer = index+2, outputValue = Just outputVal}  
        JumpIfTrue -> do
            p1 <- readModedParam 1
            p2 <- readModedParam 2
            case p1 of 
                0 -> processOpcodes $ programState {instructionPointer = index+3}
                otherwise -> processOpcodes $ programState {instructionPointer = p2}
        JumpIfFalse -> do
            p1 <- readModedParam 1
            p2 <- readModedParam 2
            case p1 of 
                0 -> processOpcodes $  programState {instructionPointer = p2}
                otherwise -> processOpcodes $ programState {instructionPointer = index+3}
        LessThan -> do
            p1 <- readModedParam 1
            p2 <- readModedParam 2
            dest <- readModedDest 3
            case p1 < p2 of
                True -> writeArray array dest 1
                False -> writeArray array dest 0
            processOpcodes $ programState {instructionPointer = index+4}
        Equals -> do
            p1 <- readModedParam 1
            p2 <- readModedParam 2
            dest <- readModedDest 3
            case p1 == p2 of
                True -> writeArray array dest 1
                False -> writeArray array dest 0
            processOpcodes $ programState {instructionPointer = index+4}
        AdjustRelativeBase -> do
            p1 <- readModedParam 1
            processOpcodes $ programState {instructionPointer = index+2, relativeBase = (relativeBase programState) + p1}
        Terminate -> do
            result <- readArray array 1
            return $  trace ("result=" ++ (show result)) programState {resultValue = Just result, terminated = True}

addInput :: ProgramState -> Integer -> ProgramState
addInput state input =state { inputValues = (inputValues state) ++ [input]}

initializeProgram :: [Integer] -> IO ProgramState
initializeProgram program = do
    arr <- newListArray  (0, toInteger 1200) (program ++ repeat 0)
    return $ ProgramState 0 arr [] 0 Nothing Nothing False

main = do
    inputText <- readFile "puzzle09_input.txt"
    let 
        program = fromJust . parseMaybe programParserP $ trim inputText

    programState <- initializeProgram program
    resultState <- processOpcodes programState
    putStrLn ("program result: " ++ show (resultValue resultState) ++ ", outputVal = " ++ show (outputValue resultState))
    