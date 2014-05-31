module Elovuon.Console
open Elovuon

let doit (t:TestTournament) =
  let sw = System.Diagnostics.Stopwatch.StartNew()
  t.Simulate()
  sw.Stop()
  printfn "%O" sw.Elapsed

[<EntryPoint>]
let main argv =
  for players, rounds in [ 10,5 ; 14,11 ; 26,9 ] do
    let file = sprintf @"..\..\..\data\T%d-%d.txt" players rounds
    let alias, contestants, rounds, results = TestTournament.Read file
    new TestTournament(alias, contestants, rounds, Algorithms.Elovuon, results) |> doit
    let alias, contestants, rounds, results = TestTournament.Read file
    new TestTournament(alias, contestants, rounds, Algorithms.SwissGraph, results) |> doit

  0 // return an integer exit code
