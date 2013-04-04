namespace Elovuon

open System
open System.Collections.Generic

type ColorPreference = bool * int

module Option =
  let choose f = function None -> None | Some x -> f x

type Contestant(player:Player, pref:ColorPreference, ?startingRank, ?rounds) =
  let rounds = defaultArg rounds 0
  let games = new List<_>()
  let mutable pref = pref
  let mutable tpr = snd player
  let mutable value = snd player
  let mutable points = 0.0
  let mutable rank = defaultArg startingRank 0
  member contestant.StartingRank with get() = defaultArg startingRank 0
  member contestant.Rank with get() = rank and set v = rank <- v
  member contestant.Name with get() = fst player
  member contestant.Elo with get() = snd player
  member contestant.Score = points
  member contestant.Tpr with get() = tpr
  member contestant.Value with get() = value
  member contestant.Pref with get() = pref
  member contestant.Offset with get() = contestant.Value - contestant.Elo
  member contestant.Order with get() = - value, - points
  member contestant.HasPlayed (other: Contestant) =
    games.Exists (new Predicate<_>(fst >> (=) other))
  member contestant.Play (other: Contestant) (black: bool) (score: float) =
    games.Add (other, (black, score))
    pref <- match pref with
            | _, 0 -> not black, 1
            | c, 1 when c = black -> not c, 0
            | c, n when c = black -> c, n - 1
            | c, n -> c, n + 1
    let played = games |> Seq.map (fun (o:Contestant,(b,s)) -> o.Elo, b, s)
    tpr <- Stats.tpr played
    value <-
      let expected = Seq.init (rounds - games.Count |> max 0) (fun _ -> contestant.Elo, false, Stats.score contestant.Elo contestant.Elo)
      Seq.append played expected
      |> Stats.tpr
    points <- points + score
  member contestant.Spread
    with get() =
      if games.Count = 0 then 0 else
      let elos = games |> Seq.map (fun (o,_) -> o.Elo)
      Seq.max elos - Seq.min elos
  member contestant.Match
    with get() =
      if games.Count = 0 then 0 else
      let elos = games |> Seq.map (fun (o,_) -> float o.Elo)
      (Seq.average elos |> Math.Round |> int) - contestant.Tpr
  member contestant.Competition
    with get() =
      if games.Count = 0 then Double.NaN else
      games |> Seq.averageBy (fun (other,_) -> contestant.Rank - other.Rank |> abs |> float)
  static member Zero with get() = new Contestant((Guid.NewGuid().ToString(),0), (false,0))
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
  override contestant.ToString() = sprintf "%-20s %3d" contestant.Name contestant.StartingRank