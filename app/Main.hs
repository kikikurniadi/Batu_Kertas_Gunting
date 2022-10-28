module Main (main) where

import Lib

playGame :: Strategy -> [Move] -> IO ()
playGame strategy moves = 
    do { putStrLn "Enter Move (Rock,Paper,Scissors,Quit): ";
        inp <- getLine;
        putStrLn $ "AI Plays: " ++ (show $ strategy moves);
        case inp of
            "Rock" -> playGame strategy (Rock:moves)
            "Paper" -> playGame strategy (Paper:moves)
            "Scissors" -> playGame strategy (Scissors:moves)
            _ -> return () }
main :: IO ()
main = do playGame complexStrategy []
          putStrLn "Game Over!"