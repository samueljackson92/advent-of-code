{- 
--- Day 5: Doesn't He Have Intern-Elves For This? ---

Santa needs help figuring out which strings in his text file are naughty or nice.

A nice string is one with all of the following properties:

It contains at least three vowels (aeiou only), like aei, xazegov, or aeiouaeiouaeiou.
It contains at least one letter that appears twice in a row, like xx, abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
It does not contain the strings ab, cd, pq, or xy, even if they are part of one of the other requirements.
For example:

ugknbfddgicrmopn is nice because it has at least three vowels (u...i...o...), a double letter (...dd...), and none of the 
disallowed substrings.
aaa is nice because it has at least three vowels and a double letter, even though the letters used by different rules overlap.
jchzalrnumimnmhp is naughty because it has no double letter.
haegwjzuvuyypxyu is naughty because it contains the string xy.
dvszwmarrgswjxmb is naughty because it contains only one vowel.
How many strings are nice?


--- Part Two ---

Realizing the error of his ways, Santa has switched to a better model of determining whether a string is naughty or nice. None of the old rules apply, as they are all clearly ridiculous.

Now, a nice string is one with all of the following properties:

It contains a pair of any two letters that appears at least twice in the string without overlapping, like xyxy (xy) or aabcdefgaa (aa), but not like aaa (aa, but it overlaps).
It contains at least one letter which repeats with exactly one letter between them, like xyx, abcdefeghi (efe), or even aaa.
For example:

qjhvhtzxzqqjkmpb is nice because is has a pair that appears twice (qj) and a letter that repeats with exactly one letter between them (zxz).
xxyxx is nice because it has a pair that appears twice and a letter that repeats with one between, even though the letters used by each rule overlap.
uurcxstgmygtbstg is naughty because it has a pair (tg) but no repeat with a single letter between them.
ieodomkazucvgmuy is naughty because it has a repeating letter with one between (odo), but no pair that appears twice.
How many strings are nice under these new rules?

-}

import System.Environment
import Data.List
import Data.List.Split
import qualified Data.Text as T

main :: IO()
main = do
	args <- getArgs
	content <- readFile $ (args !! 0)
	let ruleset = (args !! 1)
	putStrLn . show . length . filterNiceStrings ruleset . lines $ content

filterNiceStrings :: String -> [String] -> [String]
filterNiceStrings ruleset x
	| ruleset == "one" = filter niceFilter x
	| ruleset == "two" = filter niceFilter2 x
	where
		niceFilter x = (hasAtLeast3Vowels x) && (hasDoubles x) && (hasNoSubstrings x)
		niceFilter2 x = (hasAtLeast2Pairs x) && (hasRepeatedSubstring x)

-- Part One Filters
hasAtLeast3Vowels :: String -> Bool
hasAtLeast3Vowels = (<=) 3 . length . filter (\x -> elem x "aeiou")

hasDoubles :: String -> Bool
hasDoubles [] = False
hasDoubles (x:[]) = False
hasDoubles (x:y:xs) 
	| x == y    = True
	| otherwise = hasDoubles (y:xs) 

hasNoSubstrings :: String -> Bool
hasNoSubstrings [] = True
hasNoSubstrings (x:[]) = True
hasNoSubstrings (x:y:xs)
	| elem [x,y] substrs = False
	| otherwise 	     = hasNoSubstrings (y:xs) 
	where
		substrs = ["ab", "cd", "pq", "xy"]

-- Part Two Filters
hasAtLeast2Pairs :: String -> Bool
hasAtLeast2Pairs (x:y:xs)
	| countSubstrings [x,y] (x:y:xs) > 1 = True
	| otherwise = hasAtLeast2Pairs (y:xs) 
	where
		countSubstrings :: String -> String -> Int
		countSubstrings sub s = length $ T.breakOnAll (T.pack sub) (T.pack s)
hasAtLeast2Pairs _ = False

hasRepeatedSubstring :: String -> Bool
hasRepeatedSubstring (x:y:z:xs)
	| x == z    = True
	| otherwise = hasRepeatedSubstring (y:z:xs) 
hasRepeatedSubstring _ = False
