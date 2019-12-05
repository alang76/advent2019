import Data.Char

start = 130254
end = 678275

isPassword :: Int -> Bool
isPassword = (\digits -> isAscendingAndHasDouble digits && 2 `elem` (getSequences digits)) . (map digitToInt) . show

main :: IO ()
main = print . length $ filter isPassword [start..end]

getSequences :: [Int] -> [Int]
getSequences digits = map snd $ getSequenceStep digits []
    where
        getSequenceStep :: [Int] -> [(Int,Int)] -> [(Int,Int)]
        getSequenceStep [] seq = seq
        getSequenceStep (n:rest) [] = getSequenceStep rest [(n,1)]
        getSequenceStep (n:rest) ((sn,sc):seqrest)
            | n==sn = getSequenceStep rest ((sn,sc+1):seqrest)
            | otherwise = getSequenceStep rest ((n,1):(sn,sc):seqrest)


isAscendingAndHasDouble :: [Int] -> Bool
isAscendingAndHasDouble digits = 
    let
        isAscendingAndHasDoubleStep :: [Int] -> Bool -> Bool
        isAscendingAndHasDoubleStep (n:[]) foundDouble  = foundDouble
        isAscendingAndHasDoubleStep (f:n:rest) foundDouble 
            | f<=n = isAscendingAndHasDoubleStep (n:rest) (foundDouble || f==n)
            | otherwise = False
    in
        isAscendingAndHasDoubleStep digits False