#r @"..\Elovuon.Core\bin\Release\Elovuon.Core.dll"

open System
open System.IO
open Elovuon.Stats

let rng =
  let r = new Random(0)
  r.NextDouble

let tournament players rounds =
  use out = sprintf "../data/T%d-%d.txt" players rounds |> File.CreateText
  fprintfn out "%d %d" players rounds
  let ratings =
    [| for i in 1..players -> 18E2 + 1E3 * rng() |> Math.Round |> int |]
    |> Array.sortBy (~-)
  ratings |> Array.iteri ((+) 1 >> fprintfn out "%d %d")

  for i in 1..players do
    for j in 1..players do
      if i <> j then
        simulate ratings.[i-1] ratings.[j-1]
        |> fprintfn out "%d-%d %3.1f" i j

for players, rounds in [ 10,5 ; 13,7 ; 14,11 ; 26,9 ] do
  tournament players rounds