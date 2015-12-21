{---- Day 6: Probably a Fire Hazard ---

Because your neighbors keep defeating you in the holiday house decorating contest year after year, you've decided to deploy one
million lights in a 1000x1000 grid.

Furthermore, because you've been especially nice this year, Santa has mailed you instructions on how to display the ideal lighting configuration.

Lights in your grid are numbered from 0 to 999 in each direction; the lights at each corner are at 0,0, 0,999, 999,999, and 999,0.
The instructions include whether to turn on, turn off, or toggle various inclusive ranges given as coordinate pairs.
Each coordinate pair represents opposite corners of a rectangle, inclusive; a coordinate pair like 0,0 through 2,2 therefore
refers to 9 lights in a 3x3 square. The lights all start turned off.

To defeat your neighbors this year, all you have to do is set up your lights by doing the instructions Santa sent you in order.

For example:

turn on 0,0 through 999,999 would turn on (or leave on) every light.
toggle 0,0 through 999,0 would toggle the first line of 1000 lights, turning off the ones that were on, and turning on the ones
that were off.
turn off 499,499 through 500,500 would turn off (or leave off) the middle four lights.

--- Part Two ---

You just finish implementing your winning light pattern when you realize you mistranslated Santa's message from Ancient Nordic Elvish.

The light grid you bought actually has individual brightness controls; each light can have a brightness of zero or more. The lights all start at zero.

The phrase turn on actually means that you should increase the brightness of those lights by 1.

The phrase turn off actually means that you should decrease the brightness of those lights by 1, to a minimum of zero.

The phrase toggle actually means that you should increase the brightness of those lights by 2.

What is the total brightness of all lights combined after following Santa's instructions?

For example:

turn on 0,0 through 0,0 would increase the total brightness by 1.
toggle 0,0 through 999,999 would increase the total brightness by 2000000.

-}

import System.Environment
import Data.List
import Data.List.Split
import Data.String.Utils
import Data.QuadTree

data Light = OFF | ON deriving (Show, Eq)
data Instruction = Instruction {action :: String, range :: Region }  deriving (Show)

main :: IO ()
main = do
    args <- getArgs
    content <- readFile (args !! 0)
    let 
        tree = makeTree (1000, 1000) OFF
    putStrLn . show . countLightsOn . processInstructions tree . map parseInstruction . lines $ content

countLightsOn :: QuadTree Light -> Int
countLightsOn = length . filterTree (==ON) 

processInstructions :: QuadTree Light -> [Instruction] -> QuadTree Light
processInstructions t xs = foldl processInstruction t xs

processInstruction :: QuadTree Light -> Instruction -> QuadTree Light
processInstruction t (Instruction command range)
    | command == "turn on"  = turnRegionOn range t
    | command == "turn off" = turnRegionOff range t
    | command == "toggle"   = toggleRegion range t
    | otherwise = error "Malformed Command"

turnRegionOn :: Region -> QuadTree Light -> QuadTree Light
turnRegionOn range t = applyToRegion range turnOn t   

turnRegionOff :: Region -> QuadTree Light -> QuadTree Light
turnRegionOff range t = applyToRegion range turnOff t   

toggleRegion :: Region -> QuadTree Light -> QuadTree Light
toggleRegion range t = applyToRegion range toggleLights t
   where
        toggleLights t index
                | (getLocation index t) == OFF = turnOn t index
                | otherwise = turnOff t index

turnOn :: QuadTree Light -> Location -> QuadTree Light
turnOn t index = setLocation index ON t

turnOff :: QuadTree Light -> Location -> QuadTree Light
turnOff t index = setLocation index OFF t

applyToRegion :: Region -> (QuadTree a -> Location -> QuadTree a) ->  QuadTree a -> QuadTree a
applyToRegion region f t = foldl f t (indicies region)
   where
        indicies :: Region -> [Location]
        indicies (x,y,w,h) = [(i,j) | i <- [x..w], j <- [y..h]]

parseInstruction :: String -> Instruction
parseInstruction s = createInstruction . convertRange . parseParts $ s
    where
        createInstruction :: (String, Region) -> Instruction
        createInstruction (action, range) = Instruction { action = action, range = range }

convertRange :: (String, String) -> (String, Region)
convertRange (x, s) = (x, (tuplify . map read . splitOn "," . replace "through" "," $ s))
    where
        tuplify :: [Int] -> Region
        tuplify [a,b,c,d] = (a,b,c,d)

parseParts :: String -> (String, String)
parseParts s
    | (head parts) == "turn" = parseSubParts parts 2
    | otherwise = parseSubParts parts 1
    where
        parseSubParts :: [String] -> Int -> (String, String)
        parseSubParts sub n = ((intercalate " " (take n sub)), (intercalate " " (drop n sub)))
        parts = words s
