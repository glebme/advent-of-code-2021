import System.Environment
import System.IO

data Coordinate = Part1 Int Int | Part2 Int Int Int
instance Show Coordinate where
    show (Part1 x d) = concat ["Horizontal position is ", show x, ".\nDepth is ", show d, ".\nMultiplying: ", show (x * d)]
    show (Part2 x d a) = concat ["Horizontal position is ", show x, ".\nDepth is ", show d, ".\nAim is ", show a, ".\nMultiplying: ", show (x * d)]

main :: IO ()
main = do
    (fileName: args) <- getArgs
    withFile fileName ReadMode (\handle -> do
        content <- hGetContents handle
        let instructions = parseInstructions $ lines content
        putStrLn . show $ finalPosition (Part1 0 0) instructions
        putStrLn . show $ finalPosition (Part2 0 0 0) instructions)

parseInstructions :: [String] -> [(String, Int)]
parseInstructions = map ((\[x,y] -> (x, read y)) . words)

finalPosition :: Coordinate -> [(String, Int)] -> Coordinate
finalPosition startingPosition xs = foldl (\position newInstruction -> addInstructions position newInstruction) startingPosition xs

addInstructions ::  Coordinate -> (String, Int) -> Coordinate
addInstructions (Part1 x d) (direction, value)   | direction == "forward" = Part1 (x + value) d
                                                 | direction == "up" = Part1 x (d - value)
                                                 | direction == "down" = Part1 x (d + value)
                                                 | otherwise = undefined
addInstructions (Part2 x d a) (direction, value) | direction == "forward" = Part2 (x + value) (d + (a*value)) a
                                                 | direction == "up" = Part2 x d (a - value)
                                                 | direction == "down" = Part2 x d (a + value)
                                                 | otherwise = undefined