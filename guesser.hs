--  File      : Guesser.hs
--  Author    : Bryant Anderson Ciputra

--  This file describes a module that contains functions used to make the most optimal guesses for 
--  the battleship - like game, until the correct target is found. 

--  The game involves a 4x8 board, and a target consisting of 3 locations from
--  the board. Each guess consists of 3 locations, and the player continually
--  makes guesses, until the correct locations from the target are guessed, 
--  in any order. The program passes around a Game State, which contains the 
--  list of possible remaining targets. Each time the feedback for a guess is
--  returned, all elements in the game state that are inconsistent with the
--  feedback are removed from the state. From this filtered state, the guess
--  that leaves the least remaining targets is chosen to be the next guess.

module Guesser (Location, toLocation, fromLocation, feedback,
              GameState, initialGuess, nextGuess) where

import Data.List
import Data.Function
              
type GameState = [[Location]]

type Location = (Char, Int) 

-- "initialGuess" takes no input, and returns the initial guess and an initial 
-- game state, which is the list of all possible targets on the map
initialGuess :: ([Location], GameState)   
initialGuess = ([('A',2), ('E',2), ('H',4)], 
        sublists 3 [(x, y) | x <- ['A'..'H'], y <- [1..4]])
 
-- "nextGuess" takes in the previous guess and game state, and the feedback
-- to the previous guess. it returns a new guess and game state, using the 
-- information to compute the guess that leaves the least number of targets
nextGuess :: ([Location], GameState) -> (Int,Int,Int) -> ([Location], GameState)
nextGuess (curLoc, curState) curFb = (newLoc, newState)
    where
        newState = removeInconsistent curLoc curFb curState
        expectedValues = [(expectedRemaining curLoc target newState, target) 
                            | target <- newState] 
        newLoc = snd (minimumBy (compare `on` fst) expectedValues)

-- "removeInconsistent" takes in a list of Locations "guess", an integer
-- 3 tuple "fb", and a GameState "state". it filters out any element in "state"
-- whose feedback with "guess" is inconsistent to "fb", returning the newly
-- filtered state
removeInconsistent :: [Location] -> (Int,Int,Int) -> GameState -> GameState
removeInconsistent guess fb state
    = filter (\target -> feedback target guess == fb) state
 
-- "expectedRemaining" takes in two lists of Locations "guess" and "target", a
-- GameState "state", and returns the length of "state" after inconsistent
-- values have been removed from it, converted to a Float
expectedRemaining :: [Location] -> [Location] -> GameState -> Float
expectedRemaining guess target state = expectedValue
    where 
        expectedValue = fromIntegral (length 
            (removeInconsistent target (feedback guess target) state))

-- "sublists" takes two arguments, "n" and "xs". "n" is the length of each
-- sublist that will be produced from the original list "xs". this function 
-- relies on its helper function, defined below
sublists :: Int -> [Location] -> [[Location]]
sublists n xs = sublistsHelper n xs [] []

-- "sublistsHelper" is a helper function for the "sublists" function. "n" is 
-- the current length of the sublist being constructed from the list "(x:xs)", 
-- "prefix" is the current prefix of the sublist being constructed. all the 
-- sublists constructed so far are stored in the returned value "results"
sublistsHelper :: Int -> [Location] -> [Location] -> [[Location]] 
                    -> [[Location]]
sublistsHelper 0 _ prefix results = reverse prefix : results
sublistsHelper _ [] prefix results = results
sublistsHelper n (x:xs) prefix results =
   sublistsHelper (n - 1) xs (x:prefix) (sublistsHelper n xs prefix results)

-- "toLocation" converts an argument of type String "s" to type Location, if
-- the argument is a valid location name. otherwise, it returns Nothing
toLocation :: String -> Maybe Location
toLocation s 
    | (([head s]) `elem` validCols) && ((tail s) `elem` validRows) 
    = Just (head s, rowStrToInt (tail s))
    | otherwise = Nothing
    where validCols = ["A","B","C","D","E","F","G","H"]
          validRows = ["1","2","3","4"]

-- "rowStrToInt" converts the string version of a valid row name "s", to its 
-- integer version 
rowStrToInt :: String -> Int
rowStrToInt s
    | s == "1" = 1
    | s == "2" = 2
    | s == "3" = 3
    | otherwise = 4

-- "fromLocation" converts an argument of type location into the two-character
-- string version of it
fromLocation :: Location -> String
fromLocation (x, y) = [x] ++ show y

-- "feedback" takes two lists of Locations "xs" and "(y:ys)", and returns the 
-- number of Locations in "(y:ys)" present in "xs", the number of Locations 
-- one square away from a Location in "xs", and the number of Locations two
-- squares away from a Location in "xs", respectively, in the form of an 
-- integer 3-tuple
feedback :: [Location] -> [Location] -> (Int, Int, Int)
feedback _ [] = (0,0,0)
feedback xs (y:ys) = add (guessScore xs y) (feedback xs ys)

-- "guessScore" takes a list of Locations "xs", and a single Location "y", and 
-- checks whether "y" is in "xs", one away from any Location in "xs", or two
-- away from any Location in "xs", in that order. the valid first metric found
-- is the one that is counted in the return value
guessScore :: [Location] -> Location -> (Int, Int, Int)
guessScore xs y 
    | exact /= 0 = (1,0,0)
    | oneAway /= 0 = (0,1,0)
    | twoAway /= 0 = (0,0,1)
    | otherwise = emptyScore
    where emptyScore = (0,0,0)
          exact = exactScore xs y
          oneAway = oneAwayScore xs y
          twoAway = twoAwayScore xs y

-- "exactScore" takes a list of Locations "xs", and a single Location "y", 
-- returning the number of Locations in "xs" that are equal to "y"
exactScore :: [Location] -> Location -> Int
exactScore xs y = length (filter (isExact y) xs)

-- "oneAwayScore" takes a list of Locations "xs", and a single Location "y", 
-- returning the number of Locations in "xs" that are one square away from "y"
oneAwayScore :: [Location] -> Location -> Int
oneAwayScore xs y = length (filter (isOneAway y) xs)

-- "twoAwayScore" takes a list of Locations "xs", and a single Location "y", 
-- returning the number of Locations in "xs" that are two squares away from "y"
twoAwayScore :: [Location] -> Location -> Int
twoAwayScore xs y = length (filter (isTwoAway y) xs)
    
-- "isExact" takes two inputs of type Location, and returns a boolean that
-- represents whether the two inputs are the same 
isExact :: Location -> Location -> Bool
isExact (x, y) (a, b) = x == a && y == b

-- "isOneAway" takes two inputs of type Location, and returns a boolean that
-- represents whether the two inputs are one square away from each other
isOneAway :: Location -> Location -> Bool
isOneAway (x, y) (a, b)
    = ((abs (b - y)) <= 1) && ((abs (fromEnum a - fromEnum x)) <= 1)

-- "isTwoAway" takes two inputs of type Location, and returns a boolean that
-- represents whether the two inputs are two squares away from each other
isTwoAway :: Location -> Location -> Bool
isTwoAway (x, y) (a, b)
    = ((abs (b - y)) <= 2) && ((abs (fromEnum a - fromEnum x)) <= 2) 

-- "add" takes two integer tuples and returns a new tuple which is the
-- addition of both 
add :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
add (x, y, z) (a, b, c) = (x + a, y + b, z + c)


