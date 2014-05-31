namespace Elovuon

type Tournament(alias:string, contestants, rounds, algorithm: Algorithm) =
  let contestants = List.ofSeq contestants
  let count = List.length contestants
  let mutable played = 0
  member tournament.Alias with get() = alias
  member tournament.Count with get() = count
  member tournament.Played with get() = played
  member tournament.Rounds with get() = rounds
  member tournament.SetResult (white:Contestant) (black:Contestant) result =
    white.SetResult black false result
    black.SetResult white true result
  member tournament.GetPairing() : (Contestant * Contestant) list =
    algorithm.GetPairing rounds played contestants

  member tournament.FinishRound() =
    played <- played + 1
    contestants
    |> List.sortWith algorithm.CompareScore
    |> Seq.zip (Seq.init (List.length contestants) id)
    |> Seq.iter (fun (rank, contestant) -> contestant.Rank <- rank)

  member tournament.PrintStandings() =
    printfn "------------------------"
    printfn "Standings after round %d" tournament.Played
    printfn "------------------------"
    printfn "%4s %4s %3s %-20s %3s %4s" "Exp" "TPR" "Pts" "Name" "Rnk" "Rat"
    printfn "------------------------"
    for contestant in contestants |> List.sortWith algorithm.CompareScore do
      printfn "%4d %4d %3.1f %-20s %4d %4d" contestant.Value contestant.Tpr contestant.Score contestant.Name contestant.StartingRank contestant.Elo
    if played = rounds then
      printfn "------------------------"
      algorithm.GetType().Name |> printfn "Evaluation of %O"
      printfn "------------------------"
      let n = 10
      [0..n]
      |> List.iter (fun i ->
         let contestant = contestants |> Seq.find (fun c -> c.Rank = i * (count - 1) / n)
         let competition = 100.0 * contestant.Competition / float count
         printfn "%O (%+5d) : %4d %+5d %6.2f%%" contestant contestant.Offset contestant.Spread contestant.Match competition)