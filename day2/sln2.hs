import System.Environment
import System.IO

data Coordinate = Coordinate Int Int
instance Show Coordinate where
    show (Coordinate x d) = concat ["Horizontal position is ", show x, ".\nDepth is ", show d, ".\nMultiplying: ", show (x * d)]

main :: IO ()
main = do
    (fileName: args) <- getArgs
    withFile fileName ReadMode (\handle -> do
        content <- hGetContents handle
        let instructions = lines content
        putStrLn . show . finalPosition $ parseInstructions instructions)

parseInstructions :: [String] -> [(String, Int)]
parseInstructions instructions =  map ((\[x,y] -> (x, read y)) . words) instructions
    
finalPosition :: [(String, Int)] -> Coordinate
finalPosition xs = foldl (\position newInstruction -> addInstructions position newInstruction) (Coordinate 0 0) xs

addInstructions ::  Coordinate -> (String, Int) -> Coordinate
addInstructions (Coordinate x d) (direction, distance) | direction == "forward" = Coordinate (x + distance) d
                                                       | direction == "up" = Coordinate x (d - distance)
                                                       | direction == "down" = Coordinate x (d + distance)
                                                       | otherwise = undefined