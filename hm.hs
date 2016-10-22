import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import Control.Monad


-- The state of the game.
-- Contains the word, chosen letters, and number of wrong guesses.
data State = State {word :: String, letters :: String, errors :: Int} deriving Show

getWord :: String
getWord = "equilibrium"

initialState = State getWord [] 0

guessLetter :: Char -> State -> State
guessLetter letter state = State (word state) (letter : (letters state)) (errors state)

guessWord :: String -> State -> State
guessWord guess state = 
   if guess /= (word state)
      then State (word state) (letters state) (succ(errors state))
      else State (word state) (letters state) (-1)

main = do
   turn initialState

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