module Ex3 where

-- Create Rock,paper, scissor game
data GameInput = Rock | Paper | Scisor deriving Show
data GameState = Winner | Loser | Tie deriving Show
newtype Player = Player {gameInput :: GameInput}


playRPSGame (Player Rock) Rock     = Tie
playRPSGame (Player Paper) Paper   = Tie
playRPSGame (Player Scisor) Scisor = Tie

playRPSGame (Player Rock) Scisor  = Winner
playRPSGame (Player Rock) Paper   = Loser

playRPSGame (Player Scisor) Rock  = Loser
playRPSGame (Player Scisor) Paper = Winner

playRPSGame (Player Paper) Rock   = Winner
playRPSGame (Player paper) Scisor = Loser
