
import Control.Monad.ST
import Data.Array.ST

input = [1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,6,1,19,1,19,5,23,2,10,23,27,2,27,13,31,1,10,31,35,1,35,9,39,2,39,13,43,1,43,5,47,1,47,6,51,2,6,51,55,1,5,55,59,2,9,59,63,2,6,63,67,1,13,67,71,1,9,71,75,2,13,75,79,1,79,10,83,2,83,9,87,1,5,87,91,2,91,6,95,2,13,95,99,1,99,5,103,1,103,2,107,1,107,10,0,99,2,0,14,0]

test1 = [1,9,10,3,2,3,11,0,99,30,40,50]
test2 = [1,0,0,0,99]
test3 = [2,3,0,3,99]
test4 = [1,1,1,4,99,5,6,0,99]
test5 = [2,4,4,5,99,0]


processOpcodes:: Int -> STArray s Int Int -> ST s Int
processOpcodes index array = do
    opCode <- readArray array index
    leftIdx <- readArray array (index+1)
    rightIdx <- readArray array (index+2)
    leftVal <- readArray array (leftIdx+1)
    rightVal <- readArray array (rightIdx+1)
    dest <- readArray array (index+3)
    case opCode of
        1 -> do
            writeArray array (dest+1) (leftVal+rightVal)
            processOpcodes (index+4) array
        2 -> do
            writeArray array (dest+1) (leftVal*rightVal)
            processOpcodes (index+4) array
        _ -> do
            result <- readArray array 1
            return result

runProgram :: [Int] -> (Int, Int) -> ST s Int
runProgram program (noun, verb) = do
    arr <- newListArray  (1,length program ) program 
    writeArray arr 2 noun
    writeArray arr 3 verb
    res <- processOpcodes 1 arr
    elems <- getElems arr
    return res

main = do
    let
        pairs = [(noun,verb) | noun <- [0..99], verb <- [0..99]]
        results = filter (\(res, pair) -> res==19690720) . map (\pair -> (runST $ runProgram input pair, pair)) $ pairs
    print results