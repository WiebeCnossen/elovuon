namespace Elovuon
open System

[<AbstractClass>]
type Algorithm() =
  let mutable played = 0
  abstract member GetScoreLine : Contestant -> string
  abstract member CompareScore : Contestant -> Contestant-> int
  abstract member GetPairing : int -> int -> Contestant list -> (Contestant * Contestant) list

[<AbstractClass>]
type internal WeightedGraphAlgorithm<'weight, 'score when 'weight : comparison and 'score : comparison>() =
  inherit Algorithm()
  abstract member GetScore: Contestant -> 'score
  override algorithm.GetScoreLine contestant = algorithm.GetScore contestant :> obj |> string
  override algorithm.CompareScore left right = compare (algorithm.GetScore left) (algorithm.GetScore left)
  abstract member MinWeight: 'weight
  abstract member MaxWeight: 'weight
  abstract member GetWeight : int -> int -> Contestant -> Contestant -> 'weight option
  override algorithm.GetPairing rounds played contestants =
    let getWeights (contestant:Contestant) =
      contestant,
      contestants
      |> List.choose (fun other ->
         if contestant = other || contestant.HasPlayed other then None else
         let weights =
           [contestant,other; other,contestant]
           |> (if contestant < other then id else List.rev)
           |> List.choose (fun (white,black) ->
              algorithm.GetWeight rounds played white black
              |> Option.map (fun weight -> weight, contestant = black))
         if List.isEmpty weights then None else
         let weight, color = List.max weights
         Some (weight, (other, color)))
      |> List.sortBy fst
      |> List.rev
    List.map getWeights contestants
    |> Pairing.getPairing algorithm.MinWeight algorithm.MaxWeight
    |> List.sortBy (fun (a,b) -> min (algorithm.GetScore a) (algorithm.GetScore b))

type internal ElovuonAlgorithm() =
  inherit WeightedGraphAlgorithm<float, int * float>()
  let getPenalty last (lb, ln) b =
    if lb = b then Some 0.0 else
    match last, ln with
    | true, 0 -> Some 0.01
    | true, 1 -> Some 0.1
    | _, 0 -> Some 0.1
    | _, 1 -> Some 0.3
    | _ -> None
  override algorithm.MinWeight with get() = Double.MinValue
  override algorithm.MaxWeight with get() = Double.MaxValue
  override algorithm.GetWeight rounds played contestant other =
    if rounds - played > 2 * abs (contestant.StartingRank - other.StartingRank) then None else
    getPenalty (played >= rounds - 2) contestant.Pref false
    |> Option.choose (fun lp ->
       getPenalty (played >= rounds - 2) other.Pref true
       |> Option.map (fun rp ->
          let info = Stats.info contestant.Value other.Value
          let penalty = lp + rp
          let offset = min (abs contestant.Offset) (abs other.Offset) |> float
          info - penalty - 0.001 * offset))
  override algorithm.GetScore (contestant:Contestant) = -contestant.Value, -contestant.Score

  type internal SwissGraphAlgorithm() =
    inherit WeightedGraphAlgorithm<bool * float, float * int>()
    let getPenalty higher (lb, ln) b =
      if lb = b then Some 0.0 else
      let d = if higher then 0.01 else -0.01
      match ln with
      | 0 -> 0.1 + d |> Some
      | 1 -> 0.23 + d |> Some
      | _ -> None
    override altorithm.MinWeight with get() = false, Double.MinValue
    override altorithm.MaxWeight with get() = true, Double.MaxValue
    override algorithm.GetWeight _ _ contestant other =
      let higher = contestant.StartingRank < other.StartingRank
      getPenalty higher contestant.Pref false
      |> Option.choose (fun lp ->
         getPenalty (not higher) other.Pref true
         |> Option.map (fun rp ->
            let rank =  contestant.StartingRank - other.StartingRank |> abs |> float
            let swiss = contestant.Score - other.Score |> abs
            let penalty = lp + rp
            false, 0.01 * rank / 50. - swiss - penalty))
    override algorithm.GetScore (contestant:Contestant) = -contestant.Score, contestant.StartingRank