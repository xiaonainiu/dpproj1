-- by Shen Yi 844373
-- 26/08/2017

-- initialguess A1 B2 C3 : 5830 guesses
-- initialGuess C2 E2 G2 : 5948 guesses
-- initialGuess F2 G2 G3 : 6051
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
nextGuess (guess, (x:xs)) (a,b,c) = (  chooseGuess (newtarget guess (x:xs) (a,b,c)) ( (length(newtarget guess (x:xs) (a,b,c))) - 1 ) ( (length(newtarget guess (x:xs) (a,b,c)) - 2) )  ,  newtarget guess (x:xs) (a,b,c)  )

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

-- best guess F2 G2 G3 6051
-- best guess A1 C2 E3 5792
-- best guess A1 B2 C3 5830
--chooseGuess :: [[String]] -> [String]
--chooseGuess [[]] = []
--chooseGuess [a] = a
--chooseGuess (a:b:xs) =
--	if avgTargetLen a (a:b:xs) > avgTargetLen b (a:b:xs)
--		then chooseGuess (b:xs)
--		else chooseGuess (a:xs)

-- same as bottom best A1 A2 A3
chooseGuess :: [[String]] -> Int -> Int -> [String]
chooseGuess [[]] idx_min idx = []
chooseGuess [a] idx_min idx = a
chooseGuess target idx_min 0 =
	if avgTargetLen (target !! idx_min) target > avgTargetLen (target !! 0) target
		then (target !! 0)
		else (target !! idx_min)
chooseGuess target idx_min idx =
	if avgTargetLen (target !! idx_min) target > avgTargetLen (target !! idx) target
		then chooseGuess target idx (idx-1)
		else chooseGuess target idx_min (idx-1)

-- best guess A1 A2 A3 7093
-- best guess F2 G2 G3 6354
-- best guess A1 C2 E3 6186
--chooseGuess :: [[String]] -> [String]
--chooseGuess target = snd $ minimum $ [(avgTargetLen assumetarget target , assumetarget)| assumetarget <- target]


avgTargetLen :: [String] -> [[String]] -> Double
avgTargetLen assumetarget [] = 0
avgTargetLen assumetarget target = getavgLen ( frequencyOfElt ( getResponseList assumetarget target ) )

getavgLen :: [Int] -> Double
getavgLen [] = 0
getavgLen (x:xs) = ( fromIntegral (x^2)) / ( fromIntegral ( length( (x:xs) ) ) )


frequencyOfElt :: [(Int,Int,Int)] -> [Int]
frequencyOfElt xs = [ length $ filter (== c) xs | c <- nub xs]

getResponseList :: [String] -> [[String]] -> [(Int,Int,Int)]
getResponseList assumetarget [] = []
getResponseList assumetarget (x:xs) = [(response x assumetarget)] ++ getResponseList assumetarget xs