namespace Elovuon

open System.IO
open System.Text.RegularExpressions

type TestTournament<'order when 'order : comparison>(alias, contestants, rounds, algorithm, results : Map<int*int,float>) =
  inherit Tournament<'order>(alias, contestants, rounds, algorithm)

  member tournament.Simulate() =
    printfn "------------------------"
    printfn "%s - %d contestants" tournament.Alias tournament.Count
    for i in 1..rounds do
      printfn "------------------------"
      printfn "Results of round %d" i
      printfn "------------------------"
      let m = tournament.GetPairing()
      m
      |> Seq.iter (fun (w,b) ->
         results.[w.StartingRank, b.StartingRank]
         |> tournament.Play w b)
      tournament.FinishRound()
      tournament.PrintStandings()

  static member Read (algorithm: Algorithm<'order>) file =
    let alias = Path.GetFileNameWithoutExtension file
    use input = File.OpenText file

    let read f1 f2 =
      let line = Regex.Match(input.ReadLine(), @"(\S+)\s+(\S+)")
      f1 line.Groups.[1].Value, f2 line.Groups.[2].Value
    let readMany f1 f2 =
      let rec inner a =
        function
        | 0 -> List.rev a
        | n -> n - 1 |> inner (read f1 f2 :: a)
      inner []

    let count, rounds = read int int
    let contestants =
      readMany id int count
      |> List.map (fun player ->
         let rank = fst player |> int
         new Contestant(player, (rank % 2 = 0, 0), rank, rounds))

    let results =
      let parse (s:string) =
        let a = s.Split('-')
        int a.[0], int a.[1]
      count * (count-1)
      |> readMany parse float
      |> Map.ofList

    new TestTournament<_>(alias, contestants, rounds, algorithm, results)