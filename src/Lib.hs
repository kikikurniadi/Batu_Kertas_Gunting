module Lib where

data Move = Rock | Paper | Scissors
        deriving (Show,Eq)
    
type Strategy = [Move] -> Move

copyCat :: Strategy
copyCat [] = Rock
copyCat (latest:_) = latest

cycleS  :: Strategy
cycleS moves = case (length moves) `mod` 3 of
                0 -> Rock
                1 -> Paper
                2 -> Scissors

alwaysRock :: Strategy
alwaysRock _ = Rock

alternate :: Strategy -> Strategy -> Strategy
alternate str1 str2 moves = 
    case (length moves) `mod` 2 of
        0 -> str1 moves
        1 -> str2 moves

switchUP :: Strategy -> Strategy
switchUP str moves = case str moves of
                            Rock -> Paper   
                            Paper -> Scissors
                            Scissors -> Rock

switchDown :: Strategy -> Strategy
switchDown str moves = switchUP (switchUP str) moves

complexStrategy :: Strategy
complexStrategy = 
    (switchUP copyCat) `alternate` (switchDown cycleS)
