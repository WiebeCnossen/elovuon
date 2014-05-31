namespace Elovuon

type Result =
  | WhiteWins
  | Draw
  | BlackWins

type Elo = int

type Player = string * Elo