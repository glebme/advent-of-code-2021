import System.Environment
import System.IO

import Data.List
import Data.Function

main :: IO ()
main = do
    (filename : args) <- getArgs
    withFile filename ReadMode (\handle -> do
        content <- hGetContents handle
        let report = lines content
        putStrLn . show $ getPowerConsumption report)

getPowerConsumption :: [String] -> Int
getPowerConsumption report = gammaRate * epsilonRate
    where
        [gammaRate, epsilonRate] = map convertBinaryStringToInt $ generateBinaryRates report

generateBinaryRates :: [String] -> [String]
generateBinaryRates xs = transpose $ map (nub .concat . sortListOfListBasedOnLength . group . sort) $ transpose xs

sortListOfListBasedOnLength :: [[a]] -> [[a]]
sortListOfListBasedOnLength = sortBy (compare `on` length)

convertBinaryStringToInt :: String -> Int
convertBinaryStringToInt [] =  0
convertBinaryStringToInt (x:xs) | x == '0' = convertBinaryStringToInt xs
                                | x == '1' = 2 ^ (length xs) + convertBinaryStringToInt xs
                                | otherwise = undefined


