namespace Elovuon
open System

[<AbstractClass>]
type Algorithm<'order when 'order : comparison>() =
  abstract member GetWeight : Tournament<'order> -> Contestant -> Contestant -> float option
  abstract member GetOrder : Contestant -> 'order

and Tournament<'order when 'order : comparison>(alias:string, contestants, rounds, algorithm: Algorithm<'order>) =
  let contestants = List.ofSeq contestants
  let count = List.length contestants
  let mutable played = 0
  member tournament.Alias with get() = alias
  member tournament.Count with get() = count
  member tournament.Played with get() = played
  member tournament.Rounds with get() = rounds
  member tournament.Play (white:Contestant) (black:Contestant) result =
    white.Play black true result
    black.Play white false (1.0 - result)
    let d = match result with 0.0 -> "0-1" | 0.5 -> "rem" | _ -> "1-0"
    printfn "%O - %O : %s" white black d
  member tournament.GetPairing() : (Contestant * Contestant) list =
    let getWeights (contestant:Contestant) =
      contestant,
      contestants
      |> List.choose (fun other ->
         if contestant = other || contestant.HasPlayed other then None else
         let weights =
           [contestant,other; other,contestant]
           |> (if contestant < other then id else List.rev)
           |> List.choose (fun (white,black) ->
              algorithm.GetWeight tournament white black
              |> Option.map (fun weight -> weight, contestant = black))
         if List.isEmpty weights then None else
         let weight, color = List.max weights
         Some (weight, (other, color)))
      |> List.sortBy (fst >> (~-))
    List.map getWeights contestants
    |> Pairing.getPairing
    |> List.sortBy (fun (a,b) -> min (algorithm.GetOrder a) (algorithm.GetOrder b))

  member tournament.FinishRound() = played <- played + 1
  member tournament.PrintStandings() =
    printfn "------------------------"
    printfn "Standings after round %d" tournament.Played
    printfn "------------------------"
    printfn "%4s %4s %3s %-20s %3s %4s" "Exp" "TPR" "Pts" "Name" "Rnk" "Rat"
    printfn "------------------------"
    for contestant in contestants |> Seq.sortBy algorithm.GetOrder do
      printfn "%4d %4d %3.1f %-20s %4d %4d" contestant.Value contestant.Tpr contestant.Score contestant.Name contestant.Rank contestant.Elo

module Algorithms =
  type ElovuonAlgorithm() =
    inherit Algorithm<int * float>()
    let getPenalty last (lb, ln) b =
      if lb = b then Some 0.0 else
      match last, ln with
      | true, 0 -> Some 0.01
      | true, 1 -> Some 0.1
      | _, 0 -> Some 0.1
      | _, 1 -> Some 0.3
      | _ -> None
    override algorithm.GetWeight tournament contestant other =
      if tournament.Rounds > 2 * abs (contestant.Rank - other.Rank) then None else
      getPenalty (tournament.Played >= tournament.Rounds - 2) contestant.Pref false
      |> Option.choose (fun lp ->
         getPenalty (tournament.Played >= tournament.Rounds - 2) other.Pref true
         |> Option.map (fun rp ->
            let info = Stats.info contestant.Value other.Value
            let penalty = lp + rp
            let offset = min (abs contestant.Offset) (abs other.Offset) |> float
            info - penalty - 0.001 * offset))
    override algorithm.GetOrder (contestant:Contestant) = -contestant.Value, -contestant.Score

  type SwissAlgorithm() =
    inherit Algorithm<float * int>()
    let getPenalty (lb, ln) b =
      if lb = b then Some 0.0 else
      match ln with
      | 0 -> Some 0.1
      | 1 -> Some 0.3
      | _ -> None
    override algorithm.GetWeight tournament contestant other =
      getPenalty contestant.Pref false
      |> Option.choose (fun lp ->
         getPenalty other.Pref true
         |> Option.map (fun rp ->
            let rank =  contestant.Rank - other.Rank |> abs |> float
            let swiss = contestant.Score - other.Score |> abs
            let penalty = lp + rp
            0.1 * rank / float tournament.Count - swiss - penalty))
    override algorithm.GetOrder (contestant:Contestant) = -contestant.Score, contestant.Rank

  let Elovuon = new ElovuonAlgorithm() :> Algorithm<int * float>
  let Swiss = new SwissAlgorithm() :> Algorithm<float * int>