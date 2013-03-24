module Elovuon.Console
open Elovuon

let sw = System.Diagnostics.Stopwatch.StartNew()

let simulate rounds alias algorithm =
  let tournament = Tests.read rounds alias algorithm
  printfn "------------------------"
  printfn "%s - %d contestants" tournament.Alias tournament.Count
  for i in 1..rounds do
    printfn "------------------------"
    printfn "Results of round %d" i
    printfn "------------------------"
    let m = tournament.GetPairing()
    m
    |> Seq.iter (fun (w,b) ->
       let r = Stats.simulate w.Elo b.Elo
       tournament.Play w b r)
    tournament.FinishRound()
    tournament.PrintStandings()
    printfn "%O ms" sw.Elapsed


[<EntryPoint>]
let main argv =
  for alias, rounds in Tests.tournaments do
    simulate rounds alias Algorithms.Swiss
    simulate rounds alias Algorithms.Elovuon

  0 // return an integer exit code
