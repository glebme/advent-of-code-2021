import System.Environment
import System.IO

main :: IO ()
main = do
    (fileName:args) <- getArgs
    withFile fileName ReadMode (\handle -> do
        contents <- hGetContents handle
        let depths = map read $ lines contents
        putStrLn . show $ numberOfAscendingSequences depths
        putStrLn . show . numberOfAscendingSequences $ rollingWindowSequences depths)

numberOfAscendingSequences :: [Int] -> Int
numberOfAscendingSequences = pairWiseCompare . reverse

pairWiseCompare :: [Int] -> Int
pairWiseCompare (x:y:ys) = if x > y then 1 + pairWiseCompare (y:ys) else pairWiseCompare(y:ys)
pairWiseCompare _ = 0

rollingWindowSequences :: [Int] -> [Int]
rollingWindowSequences xs = foldl (zipWith (+)) xs [(tail xs), tail (tail xs)]