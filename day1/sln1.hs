import System.Environment
import System.IO

main :: IO ()
main = do
    (fileName:args) <- getArgs
    withFile fileName ReadMode (\handle -> do
        contents <- hGetContents handle
        let depths = lines contents
        putStrLn . show . numberOfAscendingSequences $ map read depths)

numberOfAscendingSequences :: [Int] -> Int
numberOfAscendingSequences = pairWiseCompare . reverse

pairWiseCompare :: [Int] -> Int
pairWiseCompare (x:y:ys) = if x > y then 1 + pairWiseCompare (y:ys) else pairWiseCompare(y:ys)
pairWiseCompare _ = 0