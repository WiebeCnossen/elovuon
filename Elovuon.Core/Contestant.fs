namespace Elovuon

open System
open System.Collections.Generic

type ColorPreference = bool * int
[<AutoOpen>]
module ColorPreferences =
  let getPenalty last (lb, ln) b =
    if lb = b then Some 0.0 else
    match last, ln with
    //| true, 0 -> Some 0.01
    //| true, 1 -> Some 0.1
    | _, 0 -> Some 0.1
    | _, 1 -> Some 0.3
    | _ -> None

module Option =
  let choose f = function None -> None | Some x -> f x

type Contestant(player:Player, pref:ColorPreference, ?rank, ?rounds, ?precision) =
  let rounds = defaultArg rounds 11
  let precision = defaultArg precision 0.0
  let games = new List<_>()
  let mutable pref = pref
  let mutable value = snd player
  let mutable points = 0.0
  member contestant.Rank with get() = defaultArg rank 0
  member contestant.Name with get() = fst player
  member contestant.Elo with get() = snd player
  member contestant.Score = points
  member contestant.Value with get() = value
  member contestant.Pref with get() = pref
  member contestant.Offset with get() = contestant.Value - contestant.Elo
  member contestant.Order with get() = - value, - points
  member contestant.Play (other: Contestant) (black: bool) (score: float) =
    games.Add (other, (black, score))
    pref <- match pref with
            | _, 0 -> not black, 1
            | c, 1 when c = black -> not c, 0
            | c, n when c = black -> c, n - 1
            | c, n -> c, n + 1
    value <-
      let played = games |> Seq.map (fun (o:Contestant,(b,s)) -> o.Elo, b, s)
      let expected = Seq.init (rounds - games.Count |> max 0) (fun _ -> contestant.Elo, false, Stats.score contestant.Elo contestant.Elo)
      Seq.append played expected
      |> Stats.tpr
    points <- points + score
  static member Zero with get() = new Contestant((Guid.NewGuid().ToString(),0), (false,0))
  member private contestant.getWeight (other: Contestant) =
    if contestant = other then Seq.empty else
    if rounds - games.Count > 2 * abs (contestant.Rank - other.Rank) then Seq.empty else
    if games.Exists (new Predicate<_>(fst >> (=) other)) then Seq.empty else
    if contestant.Rank > other.Rank then [ false; true ] else  [ true; false ]
    |> Seq.choose (fun color ->
       getPenalty (games.Count >= rounds - 2) contestant.Pref color
       |> Option.choose (fun lp ->
          not color
          |> getPenalty (games.Count >= rounds - 2) other.Pref
          |> Option.map (fun rp ->
             let white,black = if color then other,contestant else contestant,other
             let info = Stats.info white.Value black.Value
             let penalty = lp + rp
             //let swiss = contestant.Score - other.Score |> abs
             //let rank =  contestant.Rank - other.Rank |> abs |> float
             //let offset = min (abs contestant.Offset) (abs other.Offset) |> float
             let weight = info - penalty
             (if precision > 0.0 then precision * Math.Round(weight / precision) else weight), (other, color))))
    |> Seq.sortBy (fst >> (~-))
    |> Seq.truncate 1
  member contestant.getWeights =
    Seq.collect contestant.getWeight >> Seq.sortBy (fst >> (~-))
  interface System.IComparable<Contestant> with
    member contestant.CompareTo (other:Contestant) = contestant.Name.CompareTo other.Name
  interface System.IEquatable<Contestant> with
    member contestant.Equals (other:Contestant) = (contestant :> System.IComparable<Contestant>).CompareTo other = 0
  interface System.IComparable with
    member contestant.CompareTo (other:obj) =
      match other with
      | :? Contestant as o -> (contestant :> System.IComparable<Contestant>).CompareTo o
      | _ -> -1
  override contestant.Equals (other:obj) =
      match other with
      | :? Contestant as o -> (contestant :> System.IEquatable<Contestant>).Equals o
      | _ -> false
  override contestant.GetHashCode() = contestant.Name.GetHashCode()
  override contestant.ToString() = sprintf "%-20s %3d" contestant.Name contestant.Rank