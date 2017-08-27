-- by Shen Yi 844373
-- 26/08/2017

--module declaration
-- module Proj1 (initialGuess, nextGuess, GameState) where

-- function initialGuess
-- initialGuess :: ([String], GameState)
-- initialGuess = (
-- 	["A1","A2","A3"],
-- 	[]
-- 	[
-- 	Guess 
-- 		Chord Note ['A'..'G'] Octave ['1'..'3']
-- 		Chord Note ['A'..'G'] Octave ['1'..'3']
-- 		Chord Note ['A'..'G'] Octave ['1'..'3']
-- 	]
-- 	)
-- function nextGuess
-- nextGuess :: ([String], GameState) -> (Int, Int, Int) -> ([String], GameState)

-- initialGuessList = [Guess]

-- type GameState
data GameState = State [[String]] deriving (Eq, Show)

-- data Guess = Guesses (String,String,String) deriving (Eq, Show)

goals = [["A1","B1","C1"],["A2","B1","C1"],["A3","B1","C1"]]

-- state functions
deleteState :: [String] -> [[String]] -> [[String]]
deleteState guess [] = []
deleteState guess goals =
	if matchState guess goals
		then (findInitOfState guess goals) ++ (findTailOfState guess goals)
		else goals

addState :: [String] -> [[String]] -> [[String]]
addState guess goals = 
	if matchState guess goals
		then goals
		else [guess] ++ goals

matchState :: [String] -> [[String]] -> Bool
matchState guess [] = False
matchState guess goals = 
	if guess == (head goals)
		then True
		else matchState guess (tail goals)

findInitOfState :: [String] -> [[String]] -> [[String]]
findInitOfState guess [] = []
findInitOfState guess goals =
	if guess == (last goals)
		then init goals
		else findInitOfState guess (init goals)

findTailOfState :: [String] -> [[String]] -> [[String]]
findTailOfState guess [] = []
findTailOfState guess goals =
	if guess == (head goals)
		then tail goals
		else findTailOfState guess (tail goals)
