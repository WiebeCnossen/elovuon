open System
open System.IO
open System.Text.RegularExpressions

let (|WhiteElo|) line =
  let m = Regex.Match(line, @"^\[WhiteElo\s+""(\d{4})""\]$")
  if m.Success then int m.Groups.[1].Value |> Some else None

let (|BlackElo|) line =
  let m = Regex.Match(line, @"^\[BlackElo\s+""(\d{4})""\]$")
  if m.Success then int m.Groups.[1].Value |> Some else None

let (|Result|) line =
  let m = Regex.Match(line, @"^\[Result\s+""(?:(?<win>1\s*-\s*0)|(?<draw>1/2\s*-\s*1/2)|(?<loss>0\s*-\s*1))""\]$")
  if not m.Success then None
  elif m.Groups.["win"].Success then Some 1.
  elif m.Groups.["draw"].Success then Some 0.5
  else Some 0.

let readPgn =
  File.ReadAllLines
  >> Seq.scan (fun (white,black,result) line ->
     try
         if String.IsNullOrEmpty line then (None, None, None) else
         match line with
         | WhiteElo (Some elo) -> (Some elo, black, result)
         | BlackElo (Some elo) -> (white, Some elo, result)
         | Result (Some score) -> (white, black, Some score)
         | _ -> (white, black, result)
     with e -> failwithf "Error on %s: %s" line e.Message) (None, None, None)
  >> Seq.scan (fun (reset, _) (white,black,result) ->
     if reset && Option.isSome white && Option.isSome black && Option.isSome result
     then false, Some (Option.get white, Option.get black, Option.get result)
     else reset || Option.isNone white && Option.isNone black && Option.isNone result, None) (true, None)
  >> Seq.choose snd
     

let games =
  Directory.GetFiles @"D:\private\protected\personal\fsharp\twic"
  |> Seq.collect readPgn
  |> Array.ofSeq

let expected max diff =
  let fideTable =
    [|  4;  11;  18;  26;  33;  40;  47;
       54;  62;  69;  77;  84;  92;  99;
      107; 114; 122; 130; 138; 146; 154;
      163; 171; 180; 189; 198; 207; 216;
      226; 236; 246; 257; 268; 279; 291; 
      303; 316; 329; 345; 358; 375; 392; 
      412; 433; 457; 485; 518; 560; 620;
      735 |]
  let a = abs diff
  let o = Array.FindLastIndex(fideTable, fun d -> d <= max && d <= a) + 1
          |> float
          |> (*) 0.01
  if a = diff then 0.5 + o else 0.5 - o

let logit intercept alpha x =
  let u = intercept + alpha * float x
  1. + System.Math.Exp -u |> (/) 1.
  
let logitR = logit 0.18912 0.00518
let logitL = logit -1.124556 -0.006044
let logitW = logit -0.684783 0.006133
let logitE diff =
  let l, w = logitL diff, logitW diff
  let s = if l + w < 1. then 1. else l + w
  let l, w = l / s, w / s
  let d = 1. - l - w |> max 0.
  0.5 * d + w

let csv() =
  use csv = File.CreateText "Elo.csv"
  fprintfn csv "white,black,diff,sum,abs,result,loss,draw,win"
  games
  |> Seq.iter (fun (white,black,result) ->
     let bit v = if result = v then 1 else 0
     let diff, sum = white - black, white + black
     let abs = diff + 36 |> abs
     fprintfn csv "%d,%d,%d,%d,%d,%3.1f,%d,%d,%d" white black diff sum abs result (bit 0.) (bit 0.5) (bit 1.))
csv()