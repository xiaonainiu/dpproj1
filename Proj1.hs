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
nextGuess (guess, (x:xs)) (a,b,c) = (  chooseGuess (newtarget guess (x:xs) (a,b,c)) ,  newtarget guess (x:xs) (a,b,c)  )

eqNth :: Eq a => Int -> [a] -> [a] -> Bool
eqNth n l1 l2 = (l1 !! n) == (l2 !! n)

response :: [String] -> [String] -> (Int,Int,Int)
response target guess = (right, rightNote, rightOctave)
  where guess'      = nub guess
        right       = length $ intersect guess' target
        num         = length guess'
        rightNote   = num - (length $ deleteFirstsBy (eqNth 0) guess' target) - right
        rightOctave = num - (length $ deleteFirstsBy (eqNth 1) guess' target) - right

newtarget :: [String] -> [[String]] -> (Int,Int,Int) -> [[String]]
newtarget guess [[]] (a,b,c) = []
newtarget guess [atarget] (a,b,c)= if response atarget guess == (a,b,c)
	then [atarget]
	else []
newtarget guess target (a,b,c) = if response (head target) guess == (a,b,c)
	then [(head target)] ++ newtarget guess (tail target) (a,b,c)
	else newtarget guess (tail target) (a,b,c)

chooseGuess :: [[String]] -> [String]
chooseGuess [[]] = []
chooseGuess [a] = a
chooseGuess (a:b:xs) =
	if avgTargetLen a (a:b:xs) (length (a:b:xs)) > avgTargetLen b (a:b:xs) (length (a:b:xs))
		then chooseGuess (b:xs)
		else chooseGuess (a:xs)

avgTargetLen :: [String] -> [[String]] -> Int -> Double
avgTargetLen assumetarget [] a = 0
avgTargetLen assumetarget target  0 = 0
avgTargetLen assumetarget target idx =
	(getLength assumetarget (target !! (idx-1)) target) + avgTargetLen assumetarget target (idx-1)

getLength :: [String] -> [String] -> [[String]] -> Double
getLength assumetarget assumeguess [] = 0
getLength assumetarget assumeguess target =
	(fromIntegral (length ( newtarget assumeguess target (response assumetarget assumeguess) ))) / (fromIntegral (length target))



