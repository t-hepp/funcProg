import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import Control.Monad


-- The state of the game.
-- Contains the word, chosen letters, and number of wrong guesses.
data State = State {word :: String, letters :: String, errors :: Int} deriving Show

-- Dummy word as stand-in, for now.
getWord :: String
getWord = "equilibrium"

initialState = State getWord [] 0

-- Called by turn when a the input is a single letter.
guessLetter :: Char -> State -> State
guessLetter letter state = 
   if letter `elem` (letters state)
      then State (word state) (letters state) (succ(errors state))
      else State (word state) (letter : (letters state)) (errors state)

-- Called by turn when a the input is a word.
guessWord :: String -> State -> State
guessWord guess state = 
   if guess /= (word state)
      then State (word state) (letters state) (succ(errors state))
      else State (word state) (letters state) (-1)

main = do
   turn initialState

-- Handles a single turn of the game, from one user input to the next.
turn :: State -> IO ()
turn st = do
   hSetBuffering stdout NoBuffering
   putStrLn (show st)
   putStrLn ("Type in a letter or guess the word.")
   input <- getLine
   when (length input == 0) (turn st)
   if length input == 1
      then turn (guessLetter (head input) st)
      else turn (guessWord input st)
   turn st