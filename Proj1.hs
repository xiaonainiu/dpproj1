-- by Shen Yi 844373
-- 26/08/2017



--module declaration
module Proj1 (initialGuess, nextGuess, GameState) where

import Data.List

-- type GameState
type GameState = [[String]]

-- initialize Note output ["A","B","C","D","E","F","G"]
initializeNote :: [String]
initializeNote = ["A","B","C","D","E","F","G"]
note = initializeNote

-- initialize Octave output ["1","2","3"]
initializeOctave :: [String]
initializeOctave = ["1","2","3"]
octave = initializeOctave

-- initialize Pitch output all pitch
initializePitch :: [String] -> [String] -> [String]
initializePitch [] [] = []
initializePitch (x:xs) [] = []
initializePitch [] [y1,y2,y3] = []
initializePitch (x:xs) [y1,y2,y3] = 
	[x ++ y1] ++ [x ++ y2] ++ [x ++ y3] ++ initializePitch xs [y1,y2,y3]
pitch = initializePitch note octave

choose :: [String] -> Int -> [[String]]
choose _ 0 = [[]]
choose [] _ = []
choose (x:xs) n = (map (\ys -> x:ys) (choose xs (n-1))) ++ (choose xs n)

-- initialize Guess output all possible guesses
initializeGameState :: [String] -> [[String]]
initializeGameState pitch = choose pitch 3
gamestate = initializeGameState pitch

-- function initialGuess
initialGuess :: ([String], GameState)
initialGuess = (["A1","B2","C3"], initializeGameState (initializePitch initializeNote initializeOctave))

-- function nextGuess
nextGuess :: ([String], GameState) -> (Int, Int, Int) -> ([String], GameState)
nextGuess (guess, state) (0,b,c) = (head (state \\ deleteList_000 guess state), state \\ (deleteList_000 guess state))
nextGuess (guess, state) (0,3,c) = (head (state \\ deleteList_030 guess state), state \\ (deleteList_030 guess state))
-- nextGuess (guess, state) (0,2,c) = (head (state \\ deleteList_020 guess state), state \\ (deleteList_020 guess state))
-- nextGuess (guess, state) (0,0,c) = (head (state \\ deleteList_00c guess state), state \\ (deleteList_00c guess state))
nextGuess (guess, state) (1,b,c) = (head (state \\ deleteList_1bc guess state), state \\ (deleteList_1bc guess state))
nextGuess (guess, state) (2,b,c) = (head (state \\ deleteList_2bc guess state), state \\ (deleteList_2bc guess state))
nextGuess (guess, state) (a,b,c) = (head (state \\ [guess]), state \\ [guess])

matchPitchGuess :: String -> [String] -> Bool
matchPitchGuess pit [] = False
matchPitchGuess pit gus =
	if (pit `elem` gus)
 		then True
 		else False

matchNoteOrOctaveGuess :: Char -> [String] -> Bool
matchNoteOrOctaveGuess n_o [] = False
matchNoteOrOctaveGuess n_o gus =
	if (n_o `elem` (gus !! 0)) || (n_o `elem` (gus !! 0)) || (n_o `elem` (gus !! 0))
		then True
		else False



deleteList_000 :: [String] -> [[String]] -> [[String]]
deleteList_000 [a,b,c] [] = []
deleteList_000 [a,b,c] (x:xs) =
	if x == [a,b,c] || (matchPitchGuess a x) || (matchPitchGuess b x) || (matchPitchGuess c x) 
		then [x] ++ deleteList_000 [a,b,c] xs
		else deleteList_000 [a,b,c] xs

deleteList_03c :: [String] -> [[String]] -> [[String]]
deleteList_03c [a,b,c] [] = []
deleteList_03c [a:as,b:bs,c:cs] (x:xs) =
	if x == [a:as,b:bs,c:cs] || (matchNoteOrOctaveGuess a x) || (matchNoteOrOctaveGuess b x) || (matchNoteOrOctaveGuess c x) 
		then [x] ++ deleteList_03c [a:as,b:bs,c:cs] xs
		else deleteList_03c [a:as,b:bs,c:cs] xs

-- deleteList_02c :: [String] -> [[String]] -> [[String]]
-- deleteList_02c [a,b,c] [] = []
-- deleteList_02c [a:as,b:bs,c:cs] (x:xs) =
-- 	if x == [a:as,b:bs,c:cs] || (matchNoteOrOctaveGuess a x) || (matchNoteOrOctaveGuess b x) || (matchNoteOrOctaveGuess c x) 
-- 		then [x] ++ deleteList_02c [a:as,b:bs,c:cs] xs
-- 		else deleteList_02c [a:as,b:bs,c:cs] xs

deleteList_1bc :: [String] -> [[String]] -> [[String]]
deleteList_1bc [a,b,c] [] = []
deleteList_1bc [a,b,c] (x:xs) =
	if x ==[a,b,c] ||
		((not (matchPitchGuess a x)) && (not (matchPitchGuess b x)) && (not (matchPitchGuess c x))) ||
		((not (matchPitchGuess a x)) && (matchPitchGuess b x) && (matchPitchGuess c x)) ||
		((matchPitchGuess a x) && (not (matchPitchGuess b x)) && (matchPitchGuess c x)) ||
		(matchPitchGuess a x) && (matchPitchGuess b x) && (not (matchPitchGuess c x))
		then [x] ++ deleteList_1bc [a,b,c] xs
		else deleteList_1bc [a,b,c] xs

deleteList_2bc :: [String] -> [[String]] -> [[String]]
deleteList_2bc [a,b,c] [] = []
deleteList_2bc [a,b,c] (x:xs) =
	if x == [a,b,c] ||
		((not (matchPitchGuess a x)) && (not (matchPitchGuess b x)) && (not (matchPitchGuess c x))) ||
		((matchPitchGuess a x) && (not (matchPitchGuess b x)) && (not (matchPitchGuess c x))) ||
		((not (matchPitchGuess a x)) && (matchPitchGuess b x) && (not (matchPitchGuess c x))) ||
		((not (matchPitchGuess a x)) && (not (matchPitchGuess b x)) && (matchPitchGuess c x))
		then [x] ++ deleteList_2bc [a,b,c] xs
		else deleteList_2bc [a,b,c] xs



-- state functions
--deleteStateByGuess :: [String] -> [[String]] -> [[String]]
--deleteStateByGuess guess [] = []
--deleteStateByGuess guess goals =
--	if matchGuessState guess goals
--		then (findInitOfState guess goals) ++ (findTailOfState guess goals)
--		else goals

--addState :: [String] -> [[String]] -> [[String]]
--addState guess goals = 
--	if matchGuessState guess goals
--		then goals
--		else [guess] ++ goals

--matchGuessState :: [String] -> [[String]] -> Bool
--matchGuessState guess [] = False
--matchGuessState guess goals = 
--	if guess == (head goals)
--		then True
--		else matchGuessState guess (tail goals)

--findInitOfState :: [String] -> [[String]] -> [[String]]
--findInitOfState guess [] = []
--findInitOfState guess goals =
--	if guess == (last goals)
--		then init goals
--		else findInitOfState guess (init goals)

--findTailOfState :: [String] -> [[String]] -> [[String]]
--findTailOfState guess [] = []
--findTailOfState guess goals =
--	if guess == (head goals)
--		then tail goals
--		else findTailOfState guess (tail goals)

--deleteStateByPitch :: String -> [[String]] ->  [[String]]
--deleteStateByPitch pit [] = []
--deleteStateByPitch pit goals =
--	if matchPitchState pit goals
--		then (findInitOfState guess goals) ++ (findTailOfState guess goals)
--		else goals
