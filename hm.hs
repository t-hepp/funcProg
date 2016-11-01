import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import Control.Monad


-- The state of the game.
-- Contains the word, chosen letters, and number of wrong guesses.
data State = State {word :: String, letters :: String, errors :: Int} deriving Show

-- Dummy word as stand-in, for now.
-- TODO read random word from list in file
getWord :: String
getWord = "equilibrium"

initialState = State getWord [] 0

-- Called by turn when the input is a single letter.
guessLetter :: Char -> State -> State
guessLetter letter state
   | letter `elem` letters state = State (word state) (letters state) (succ(errors state))
   | letter `elem` word state    = State (word state) (letter : (letters state)) (errors state)
   | otherwise                   = State (word state) (letters state) (succ(errors state))

-- Called by turn when a the input is a word.
guessWord :: String -> State -> State
guessWord guess state = 
   if guess /= (word state)
      then State (word state) (letters state) (succ(errors state))
      else State (word state) (letters state) (-1)

main = turn initialState

-- Displays the word with only chosen letters revealed.
displayWord :: State -> IO ()
displayWord state = putStrLn [obscureChar (w `elem` letters state) w| w <- word state]
 where
  obscureChar :: Bool -> Char -> Char
  obscureChar bool char
   | bool = char
   | otherwise = '-'

test :: String -> String
test xs = [succ x | x <- xs]

-- Handles a single turn of the game, from one user input to the next.
turn :: State -> IO ()
turn (State _ _ (-1)) = do
   putStrLn "\n\nCongratulations!"
   return()
turn st = do
   hSetBuffering stdout NoBuffering
   putStrLn ("\n\n" ++ show st)
   displayWord st
   putStrLn ("Type in a letter or guess the word.")
   input <- getLine
   when (length input == 0) (turn st)
   if length input == 1
      then turn (guessLetter (head input) st)
      else turn (guessWord input st)  
