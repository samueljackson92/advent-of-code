{-

Advent of Code: Day 1

Santa is trying to deliver presents in a large apartment building, but he can't find the right floor.
The directions he got are a little confusing. He starts on the ground floor (floor 0) and then follows the instructions 
one character at a time.

An opening parenthesis, (, means he should go up one floor, and a closing parenthesis, ), means he should go down one floor.

The apartment building is very tall, and the basement is very deep; he will never find the top or bottom floors.

For example:

(()) and ()() both result in floor 0.
((( and (()(()( both result in floor 3.
))((((( also results in floor 3.
()) and ))( both result in floor -1 (the first basement level).
))) and )())()) both result in floor -3.

To what floor do the instructions take Santa?
-}


-- Part one 
findFloor :: [Char] -> Int
findFloor directions = parseElements directions 0
	where 
		parseElements :: [Char] -> Int -> Int
		parseElements [] floor = floor
		parseElements (x:xs) floor
			| x == '(' = parseElements xs (floor+1)
			| x == ')' = parseElements xs (floor-1)
			| otherwise = error ("Invalid element: " ++ (show x))


{-
 - --- Part Two ---

Now, given the same instructions, find the position of the first character that causes him to enter the basement (floor -1). 
The first character in the instructions has position 1, the second character has position 2, and so on.

For example:

) causes him to enter the basement at character position 1.
()()) causes him to enter the basement at character position 5.

What is the position of the character that causes Santa to first enter the basement?
-}
findBasementEntryPosition :: [Char] -> Int
findBasementEntryPosition directions = findPosition directions 0 1
	where 
		findPosition :: [Char] -> Int -> Int -> Int
		findPosition [] floor position = error ("Santa never enters the basement")
		findPosition (x:xs) floor position
			| x == ')' && floor == 0 = position
			| x == '(' = findPosition xs (floor+1) (position+1)
			| x == ')' = findPosition xs (floor-1) (position+1)
			| otherwise = error ("Invalid element: " ++ (show x))
