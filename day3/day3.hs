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
        putStrLn . show $ co2Rating report 0
        putStrLn . show $ getPowerConsumption report
        putStrLn . show $ getLifeSupportRating report)

getPowerConsumption :: [String] -> Int
getPowerConsumption report = gammaRate * epsilonRate
    where
        [gammaRate, epsilonRate] = map convertBinaryStringToInt $ generatePowerBinaryRates report

generatePowerBinaryRates :: [String] -> [String]
generatePowerBinaryRates xs = transpose $ map (nub .concat . sortListOfListBasedOnLength . group . sort) $ transpose xs

sortListOfListBasedOnLength :: [[a]] -> [[a]]
sortListOfListBasedOnLength = sortBy (compare `on` length)

convertBinaryStringToInt :: String -> Int
convertBinaryStringToInt [] =  0
convertBinaryStringToInt (x:xs) | x == '0' = convertBinaryStringToInt xs
                                | x == '1' = 2 ^ (length xs) + convertBinaryStringToInt xs
                                | otherwise = undefined

getLifeSupportRating :: [String] -> Int
getLifeSupportRating report = (convertBinaryStringToInt $ oxygenRating report 0) * (convertBinaryStringToInt $ co2Rating report 0)

oxygenRating :: [String] -> Int -> String
oxygenRating [x] _ = x
oxygenRating xs position | length zeros == 0 && length ones == 0 = ""
                         | length zeros > length ones = oxygenRating zeros (position+1)
                         | length zeros <= length ones = oxygenRating ones (position+1)
    where
        (zeros, ones) = headFilter xs position 

co2Rating :: [String] -> Int -> String
co2Rating [x] _ = x
co2Rating xs position | length zeros == 0 && length ones == 0 = ""
                      | length zeros <= length ones = co2Rating zeros (position+1)
                      | length zeros > length ones = co2Rating ones (position+1)
    where
        (zeros, ones) = headFilter xs position 


headFilter :: [String] -> Int -> ([String], [String])
headFilter [] _ = ([], [])
headFilter xs@(y:ys) position = if (length y == position)
    then ([],[])
    else partition (\value -> if (value!!position) == '0' then True else False) xs 
