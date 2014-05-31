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
  override algorithm.CompareScore left right = compare (algorithm.GetScore left) (algorithm.GetScore right)
  abstract member MinWeight: 'weight
  abstract member MaxWeight: 'weight
  abstract member GetWeight : int -> int -> Contestant -> Contestant -> 'weight option
  abstract member InitializePairing : Contestant list -> unit
  default algorithm.InitializePairing contestants = ()
  override algorithm.GetPairing rounds played contestants =
    algorithm.InitializePairing contestants
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
    inherit WeightedGraphAlgorithm<float * bool * bool * int * float * int, float * int>()
    override algorithm.MinWeight with get() = Double.MinValue, false, false, Int32.MinValue, Double.MinValue, Int32.MinValue
    override algorithm.MaxWeight with get() = Double.MaxValue, true, true, Int32.MaxValue, Double.MaxValue, Int32.MaxValue
    override algorithm.GetWeight rounds played contestant other =
      let topScorer =
        (rounds = played + 1) &&
        (contestant.Score > float played / 2. || other.Score > float played / 2.)
      let prefs = [ contestant.Pref, false; other.Pref, true ]
      let absolutePref =
        prefs
        |> List.forall (fun ((pc,pn), c) -> pc = c || pn < 2)
      if not absolutePref && not topScorer then None else
      let otherPref =
        prefs
        |> List.forall (fun ((pc,pn), c) -> pc = c)
      let samePref = fst contestant.Pref = fst other.Pref
      (
        contestant.Score - other.Score |> abs |> (-) (float played),
        absolutePref,
        otherPref,
        contestant.GroupRank - other.GroupRank |> abs,
        contestant.Score,
        contestant.StartingRank
      )
      |> Some
    override algorithm.GetScore (contestant:Contestant) = -contestant.Score, contestant.StartingRank
    override algorithm.InitializePairing contestants =
      contestants
      |> Seq.groupBy (fun c -> c.Score)
      |> Seq.map snd
      |> Seq.collect (fun s ->
         let l = Seq.length s
         s
         |> Seq.sortBy (fun c -> c.StartingRank)
         |> Seq.zip (Seq.init l ((+) 1)))
      |> Seq.iter (fun (i,c) -> c.GroupRank <- i)