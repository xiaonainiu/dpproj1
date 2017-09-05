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
initializePitch (x:xs) [y1,y2,y3] = [x ++ y1] ++ [x ++ y2] ++ [x ++ y3] ++ initializePitch xs [y1,y2,y3]
pitch = initializePitch note octave

-- function for select pitches to component a guess
choose :: [String] -> Int -> [[String]]
choose _ 0 = [[]]
choose [] _ = []
choose (x:xs) n = (map (\ys -> x:ys) (choose xs (n-1))) ++ (choose xs n)

-- initialize Guess output all possible guesses
initializeGameState :: [String] -> [[String]]
initializeGameState pitch = choose pitch 3
gamestate = initializeGameState pitch

-- function initialGuess
-- Give the first guess A1 C2 E3, with the initial target
initialGuess :: ([String], GameState)
initialGuess = (["A1","C2","E3"], initializeGameState (initializePitch initializeNote initializeOctave))

-- function nextGuess
-- Use the previous game state and the feedback to dicide the new target and new guess
nextGuess :: ([String], GameState) -> (Int, Int, Int) -> ([String], GameState)
nextGuess (guess, (x:xs)) (a,b,c) = (  chooseGuess (newtarget guess (x:xs) (a,b,c))  ,  newtarget guess (x:xs) (a,b,c)  )
-- nextGuess (guess, (x:xs)) (a,b,c) = (  chooseGuess (newtarget guess (x:xs) (a,b,c)) ( (length(newtarget guess (x:xs) (a,b,c))) - 1 ) ( (length(newtarget guess (x:xs) (a,b,c)) - 2) )  ,  newtarget guess (x:xs) (a,b,c)  )

-- Input the target and guess, output the feedback, same result with response in the test file
getResponse :: [String] -> [String] -> (Int,Int,Int)
getResponse target guess = (right,rightNote,rightOctave) 
  where right      = 3 - (length $ target\\guess)
        target_note = map head target
        guess_note = map head guess
        target_octave = map last target
        guess_ocatve = map last guess
        rightNote = 3 - (length $ target_note\\guess_note) - right
        rightOctave = 3 - (length $ target_octave\\guess_ocatve) - right

-- Get the new target, compare previous guess with all elements in previous guess use the getResponse function.
-- If the feedback is same with the feedback which system provide, the element will be insert into the new target.
newtarget :: [String] -> [[String]] -> (Int,Int,Int) -> [[String]]
newtarget guess [[]] (a,b,c) = []
newtarget guess [atarget] (a,b,c)= if getResponse atarget guess == (a,b,c)
	then [atarget]
	else []
newtarget guess target (a,b,c) = if getResponse (head target) guess == (a,b,c)
	then [(head target)] ++ newtarget guess (tail target) (a,b,c)
	else newtarget guess (tail target) (a,b,c)

-- Choose a guess in the new target.
-- For each elements, get the expect length of it, if we use it as the next guess. Choose the shortest one.
-- This is the most important step, try different first guess, get different times of guess.
-- Best guess is 5792 times in total
-- best guess F2 G2 G3 6051
-- best guess A1 C2 E3 5792
-- best guess A1 B2 C3 5830
chooseGuess :: [[String]] -> [String]
chooseGuess [[]] = []
chooseGuess [a] = a
chooseGuess (a:b:xs) =
	if avgTargetLen a (a:b:xs) > avgTargetLen b (a:b:xs)
		then chooseGuess (b:xs)
		else chooseGuess (a:xs)

-- Another realize of chooseGuess, less efficient, but better in format.
-- For the test, I choose the one which use less times guess.
-- best A1 A2 A3
-- best A1 B2 C3 6247
-- chooseGuess :: [[String]] -> Int -> Int -> [String]
-- chooseGuess [[]] idx_min idx = []
-- chooseGuess [a] idx_min idx = a
-- chooseGuess target idx_min 0 =
-- 	if avgTargetLen (target !! idx_min) target > avgTargetLen (target !! 0) target
-- 		then (target !! 0)
-- 		else (target !! idx_min)
-- chooseGuess target idx_min idx =
-- 	if avgTargetLen (target !! idx_min) target > avgTargetLen (target !! idx) target
-- 		then chooseGuess target idx (idx-1)
-- 		else chooseGuess target idx_min (idx-1)

-- Get the average length of new target if we choose this guess
avgTargetLen :: [String] -> [[String]] -> Double
avgTargetLen assumetarget [] = 0
avgTargetLen assumetarget target = getavgLen ( frequencyOfElt ( getResponseList assumetarget target ) )

-- Get the length of length of new target 
getavgLen :: [Int] -> Double
getavgLen [] = 0
getavgLen (x:xs) = ( fromIntegral (x^2)) / ( fromIntegral ( length( (x:xs) ) ) )

-- Get the frequency of each kind of feedback in the list
frequencyOfElt :: [(Int,Int,Int)] -> [Int]
frequencyOfElt xs = [ length $ filter (== c) xs | c <- nub xs]

-- Get a list of response
getResponseList :: [String] -> [[String]] -> [(Int,Int,Int)]
getResponseList assumetarget [] = []
getResponseList assumetarget (x:xs) = [(getResponse x assumetarget)] ++ getResponseList assumetarget xs