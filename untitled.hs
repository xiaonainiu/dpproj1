-- by Shen Yi 844373
-- 26/08/2017

--module declaration
module Proj1 (initialGuess, nextGuess, GameState) where

-- function initialGuess
initialGuess :: ([String], GameState)


-- function nextGuess
nextGuess :: ([String], GameState) -> (Int, Int, Int) -> ([String], GameState)


-- type GameState
type GameState = [(Guess_history)]

-- data Guses_history
data Guess_history = 
	Guess
	|Feedback Int Int Int

data Guess Chord Chord Chord

data Chord = 
	Note
	|Octave

data Note Char

data Octave Char