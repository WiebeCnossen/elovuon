module Elovuon.Stats
open System

let private logit intercept alphas xs =
  let u =
    List.zip alphas xs
    |> List.fold (fun a (alpha,x) -> a + alpha * float x) intercept
  1. + Math.Exp -u |> (/) 1.

let private random =
  let seed = (new Random()).Next 100000
  printfn "Seed = %d" seed
  let rng = new Random(seed)
  rng.NextDouble

let rec private binarySearch estimator (low, dl) (high, dh) =
  let mid = (low + high) / 2
  if low = mid then
    if abs dl < dh then low else high
  else
  let dm = estimator mid
  if dm = 0. then mid
  elif dm > 0. then binarySearch estimator (low, dl) (mid, dm)
  else binarySearch estimator (mid, dm) (high, dh)

let private scores white black =
  let diff = white - black
  let sum = white + black
  let abs = diff + 36 |> abs
  let loss = logit 1.3193154406 [-0.0059289875; -0.0005258458] [diff; sum]
  let draw = logit -2.9182278307 [-0.0046478248; 0.0006298267] [abs; sum]
  let win = logit 0.802351487 [0.006077632; -0.000319317] [diff; sum]
  let total = loss + draw + win
  loss / total, draw / total, win / total

let loss white black = let loss,_,_ = scores white black in loss
let draw white black = let _,draw,_ = scores white black in draw
let win white black = let _,_,win = scores white black in win
let score white black =
  let _,draw,win = scores white black
  0.5 * draw + win

let info white black =
  let log x = Math.Log(x, 2.)
  [ loss; draw; win ]
  |> List.sumBy (fun f -> let x = f white black in -x * log x)

let whitePeer elo =
  let estimator mid = 0.5 - score elo mid
  binarySearch estimator (elo, -1.) (elo + 100, 1.)
let blackPeer elo =
  let estimator mid = score mid elo - 0.5
  binarySearch estimator (elo - 100, -1.) (elo, 1.)

let simulate white black =
  let r = random()
  let l = loss white black
  if r < l then 0. else
  let w = win white black
  if r < l + w then 1.
  else 0.5

let tpr (games : (Elo * bool * float) seq) =
  if Seq.isEmpty games then failwith "No games" else
  let n, s = games |> Seq.fold (fun (n,s) (_,_,r) -> n+1, s+r) (0, 0.)
  if s = 0. then (games |> Seq.map (fun (elo,_,_) -> elo) |> Seq.min) - 720 else
  if s = float n then (games |> Seq.map (fun (elo,_,_) -> elo) |> Seq.max) + 720 else
  let estimator mid =
    let tm =
      games
      |> Seq.sumBy (fun (elo, black, _) ->
         if black
         then score elo mid |> (-) 1.
         else score mid elo)
    tm - s
  binarySearch estimator (0, -s) (4000, float n - s)