module Elovuon.Console
open Elovuon

let doit (t:TestTournament<_>) =
  let sw = System.Diagnostics.Stopwatch.StartNew()
  t.Simulate()
  sw.Stop()
  printfn "%O" sw.Elapsed

[<EntryPoint>]
let main argv =
  for players, rounds in [ 10,5 ; 14,11 ; 26,9 ] do
    let file = sprintf @"..\..\..\data\T%d-%d.txt" players rounds
    TestTournament.Read Algorithms.Elovuon file |> doit
    TestTournament.Read Algorithms.Swiss file |> doit

  0 // return an integer exit code
