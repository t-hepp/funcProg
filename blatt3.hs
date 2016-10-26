import Data.Maybe

type Id         = Int
type Name       = String
type FSK        = Int
type Movie      = (Id, Name, FSK)
type Moviestore = ( [Movie] {- verfügbare Filme-}
                  , [Movie] {- ausgeliehene Filme -})



myMovieStore :: Moviestore
myMovieStore = ( [ (1, "Matrix"                       , 16)
                 , (2, "Alpen - unsere Berge von oben",  0)
                 , (3, "m3",  0)
                 , (4, "m4",  0)
                 , (5, "m5",  0)
                 , (6, "m6",  0)
                 , (7, "m7",  0)
                 , (8, "m8",  0)
                 , (9, "m9",  0)
                 , (10, "m10",  0)
                 , (11, "m11",  0)
                 , (12, "m12",  0)
                 , (13, "m13",  0)
                 ]
               , [ (14, "The Breakfast Club"           , 12)
                 ]
               )

myMovieList :: [Movie]
myMovieList = [ (1, "Matrix", 16), (2, "Alpen - unsere Berge von oben",  0)]

showMovie :: Movie -> String
showMovie (id, titel, fsk) = titel ++ " (Id: " ++ show id ++ ", FSK " ++ show fsk ++ ")"

showMovieList :: [Movie] -> String
showMovieList []     = ""
showMovieList (m:ms) = showMovie m ++ "\n" ++ showMovieList ms

showMoviestore :: Moviestore -> String
showMoviestore (verfuegbar, ausgeliehen) = vHeader ++ newLine ++vDashes ++ (showMovieList verfuegbar) ++ newLine ++ aHeader ++ newLine ++ aDashes  ++ (showMovieList ausgeliehen)
 where vHeader = "Verfügbare Filme (" ++ (show (length verfuegbar)) ++ ")"
       aHeader = "Ausgeliehene Filme (" ++ (show (length ausgeliehen)) ++ ")"
       vHeaderLength = (length vHeader)
       aHeaderLength = (length aHeader)
       vDashes = (replicate vHeaderLength '=') ++ "\n"
       aDashes = (replicate aHeaderLength '=') ++ "\n"
       newLine = "\n"

extract :: Id -> [Movie] -> (Maybe Movie, [Movie])
extract id (ms) = (Nothing ms)

maybeMovie :: Id -> [Movie] -> Maybe Movie
maybeMovie id (ms) =
   if length list > 0
      then Just head list
      else Nothing
 where list = [m | m <- ms, (head m) == id]

