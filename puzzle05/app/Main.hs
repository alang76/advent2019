 {-# LANGUAGE OverloadedStrings #-}
 {-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.ST
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Maybe
import Data.Array.IO
import Debug.Trace
import Data.Text (strip, pack, unpack)

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

processOpcodes:: Int -> IOArray Int Int -> IO Int
processOpcodes index array = do
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
            processOpcodes (index+4) array
        Multiplication -> do
            doBinOp (*)
            processOpcodes (index+4) array
        Input -> do
            dest <- readParam 1
            putStrLn "input:"
            input <- getLine
            writeArray array dest (fromJust . parseMaybe intParserP . trim $ input)
            processOpcodes (index+2) array
        Output -> do
            outputVal <- readParamVal 1 (getFirstParam paramModes)
            putStrLn ("output:" ++ show outputVal)
            processOpcodes (index+2) array
        JumpIfTrue -> do
            p1 <- readModedParam 1
            p2 <- readModedParam 2
            case p1 of 
                0 -> processOpcodes (index+3) array
                otherwise -> processOpcodes p2 array
        JumpIfFalse -> do
            p1 <- readModedParam 1
            p2 <- readModedParam 2
            case p1 of 
                0 -> processOpcodes p2 array
                otherwise -> processOpcodes (index+3) array
        LessThan -> do
            p1 <- readModedParam 1
            p2 <- readModedParam 2
            dest <- readParam 3
            case p1 < p2 of
                True -> writeArray array dest 1
                False -> writeArray array dest 0
            processOpcodes (index+4) array
        Equals -> do
            p1 <- readModedParam 1
            p2 <- readModedParam 2
            dest <- readParam 3
            case p1 == p2 of
                True -> writeArray array dest 1
                False -> writeArray array dest 0
            processOpcodes (index+4) array
        Terminate -> do
            result <- readArray array 1
            return result
    

runProgram :: [Int] -> IO Int
runProgram program = do
    arr <- newListArray  (0, (length program)-1) program  :: IO (IOArray Int Int)
    res <- processOpcodes 0 arr
    return res

test1 = [3,9,8,9,10,9,4,9,99,-1,8]
test2 = [3,9,7,9,10,9,4,9,99,-1,8]
test3 = [3,3,1108,-1,8,3,4,3,99]
test4 = [3,3,1107,-1,8,3,4,3,99]
test5 = [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]
test6 = [3,3,1105,-1,9,1101,0,0,12,4,12,99,1]
test7 = [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]

main = do
    inputText <-readFile "puzzle05_input.txt"
    let program = fromJust . parseMaybe programParserP $ inputText
    result <- runProgram $ program 
    putStrLn ("program result: " ++ show result)
