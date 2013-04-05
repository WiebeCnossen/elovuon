module Elovuon.Tests

open Elovuon
open Elovuon.Stats

let private root = @"D:\dotnet\Elovuon\Data"
let tournaments =
  if false then
    [ "mini", 5 ]
  elif false then
    [
      "forni2012", 9
      "reykjavik2013", 3
    ]
  elif false then
    [
      "sj", 9
      "weekend2011", 6
      "forni2012", 9
      "reykjavik2013", 10
    ]
  else
    [
      "sj", 9
      "weekend2011", 6
      "forni2012", 9
      "reykjavik2013", 10
      "european2012", 11
      "cappelle2013", 9
    ]

let read rounds alias algorithm =
  let contestants =
    sprintf @"%s\%s.txt" root alias
    |> System.IO.File.ReadAllLines
    |> Array.choose (fun line ->
       match line.Split [| '\t' |] with
       | [| name; elo |] -> Some (name, int elo)
       | _ -> None)
    |> Array.mapi (fun i p -> new Contestant(p, (i % 2 = 0, 0), i + 1, rounds))
  new Tournament<_>(alias, contestants, rounds, algorithm)

let randomGraph seed m n =
  let rng = new System.Random(seed)
  let nodes = new System.Collections.Generic.HashSet<_>()
  let edges = ref 0
  let doubleEdges =
    Seq.init m id
    |> Seq.collect (fun v ->
       let w = m - 2 |> rng.Next
       let x = m - w - 2 |> rng.Next |> (+) (w + 1)
       seq {
         yield (if w >= v then w + 1 else w) |> Graph.edge v
         yield (if x >= v then x + 1 else x) |> Graph.edge v
       })
  let randomEdges =
    Seq.initInfinite (fun _ ->
      let a = m - 1 |> rng.Next
      let b = m - a - 1 |> rng.Next |> (+) (a + 1)
      a, b)

  try
    Seq.append doubleEdges randomEdges
    |> Seq.distinct
    |> Seq.takeWhile (fun (v,w) ->
       try
         !edges < n || nodes.Count < m       
       finally
         incr edges
         nodes.Add v |> ignore
         nodes.Add w |> ignore)
    |> Graph.fromEdges
  finally
    printfn "%d edges, %d nodes" (!edges - 1) nodes.Count

let nearGraph m n o =
  [0..m-1]
  |> List.collect (fun i -> [i+1+o..min (i+n+o) (m-1)] |> List.map (Graph.edge i))
  |> Graph.fromEdges


