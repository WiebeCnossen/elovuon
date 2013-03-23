module Elovuon.Console

open System
open Elovuon
open Elovuon.Stats

let infoTolerance = 0.1

let solve = Graph.findPerfectMatching

let getPrios lo hi weights =
  if Array.isEmpty weights || weights |> Array.exists (snd >> Array.isEmpty) then Array.empty else
  weights
  |> Array.collect (fun (c,a) ->
     a
     |> Array.choose (fun (w,(o,b)) ->
        if w < lo || w >= hi then None else
        let edge = Graph.edge c o
        Some ((-w, -a.Length), ((w, edge), if fst edge = c then b else not b))
        ))
  |> Seq.sort
  |> Seq.map snd
  |> Seq.distinct
  |> Seq.toArray

let weights2graph weights =
  weights
  |> Array.collect (fun (c, a) -> a |> Array.map (fun (_,(o,_)) -> Graph.edge c o))
  |> Seq.distinct
  |> Graph.fromEdges

let limit we weights =
    weights
    |> Array.map (fun (c, a) ->
        c, Array.filter (fun (w,_) -> w >= we) a)

let strip we (x,y) weights =
    weights
    |> Array.choose (fun (c, a) ->
        if c = x || c = y then None else
        Some (c, Array.filter (fun (w,(o,_)) -> w >= we && o <> x && o <> y) a))

let minWeight (weights : (Contestant * (float * (Contestant * bool)) array) array) =
  let values =
    weights
    |> Array.collect (snd >> (Array.map fst))
    |> Seq.distinct
    |> Seq.sort
    |> Array.ofSeq

  let solves w =
    if w = values.[0] then true else
    let weights = weights |> limit w
    let m = weights |> Array.map (snd >> Array.length) |> Array.max
    if weights |> Array.exists (snd >> Array.isEmpty) then false else
    printf "?"
    weights
    |> weights2graph
    |> Graph.findPerfectMatching
    |> Option.isSome
    
  let rec inner lo hi =
    let upper = if hi < values.Length then values.[hi] else Double.MaxValue
    if lo = hi - 1 then values.[lo], upper else
    let mi = (hi + lo) / 2
    if solves values.[mi] then inner mi hi else inner lo mi
  let lo,hi =
    let poor = weights |> Array.minBy (fun (_,a) -> fst a.[0], a.Length) |> snd |> Array.map fst
    Seq.append poor [values.[0]]
    |> Seq.distinct
    |> Seq.scan (fun (_,hi) lv ->
       if solves lv then Some lv, hi
       else None, System.Array.BinarySearch(values, lv)) (None, System.Array.BinarySearch(values, poor.[0]) + 1)
    |> Seq.pick (function None,_ -> None | Some lv, hi -> Some (System.Array.BinarySearch(values, lv), hi))
  printf "!"
  inner lo hi

let getPairing (contestants: Contestant array) =
  let weights =
    contestants
    |> Array.map (fun c -> c, Seq.ofArray contestants |> c.getWeights |> Array.ofSeq)

  let rec inner lower pairing =
    let paired = pairing |> List.collect (fun (o,c) -> [o;c]) |> set
    let weights =
      weights
      |> Array.choose (fun (c, a) ->
         if paired.Contains c then None else
         Some (c, Array.filter (fun (w,(o,_)) -> w >= lower && (paired.Contains o |> not)) a))
    if weights.Length = 0 then pairing else

    let rec sub we (x,y) =
      let getWeight (x,y) =
        weights
        |> Array.find (fst >> (=) x)
        |> snd
        |> Array.find (snd >> fst >> (=) y)
      let weights = strip we (x,y) weights
      if weights |> Array.exists (snd >> Array.isEmpty) then None else
      printf "."
      match weights2graph weights |> solve with
      | None -> None
      | Some matching ->
        let pairs =
          matching
          |> List.filter (fun (c,o) ->
             weights
             |> Array.exists (fun (p, a) ->
                if p <> c && p <> o then false else
                let q = if p = c then o else c
                a
                |> Seq.takeWhile (fun (w,(x,_)) -> w < we + infoTolerance)
                |> Seq.exists (fun (_,(x,_)) -> x = q)))
        List.length pairs + 1 |> printfn "*%d"
        (x,y)::pairs
        |> List.map (fun (x,y) ->
           let w,(_,black) = getWeight (x,y)
           if black then y,x else x,y)
        |> (@) pairing
        |> inner we
        |> Some

    match weights |> Array.filter (snd >> Array.length >> (=) 1) with
    | [||] ->
      let mw,uw = minWeight weights
      match getPrios mw uw weights with
      | [| (_,(x,y)),black |] ->
        printfn "*1"
        (if black then y,x else x,y) :: pairing |> inner mw
      | prios -> prios |> Array.map fst |> Array.pick (fun (w,e) -> sub mw e)
    | a ->
      let pairs =
        a
        |> Seq.map (fun (c,w) -> let _,(o,b) = w.[0] in if b then o,c else c,o)
        |> Seq.distinct
        |> List.ofSeq
      List.length pairs |> printfn "*%d"
      pairs @ pairing |> inner lower

  inner Double.MinValue []

[<EntryPoint>]
let main argv =
#if doh
  let g =
    [( 18081847, [ 18421909; 18261870; 17511786; 17961846]);
     ( 18421909, [ 18081847; 18741823; 18921875; 18621829]);
     ( 18741823, [ 18421909; 18921875; 18621829; 19111760]);
     ( 18921875, [ 18421909; 18621829; 19111760; 18741823]);
     ( 18621829, [ 18421909; 18921875; 19111760; 18741823]);
     ( 19111760, [ 18921875; 18621829; 18741823]);
     ( 17961846, [ 18261870; 18081847; 17511786]);
     ( 18261870, [ 17961846; 18081847]);
     ( 17511786, [ 18081847; 17961846; 17211819; 17121747]);
     ( 17211819, [ 17511786; 16721655; 17121747]);
     ( 16721655, [ 17211819; 17121747]);
     ( 17121747, [ 17211819; 17511786; 16721655])]
  solve g |> printfn "%A"
  let g =
    [( 21992276, [ 21712268]);
     ( 21712268, [ 21992276]);
     ( 22452245, [ 22682230]);
     ( 22682230, [ 22452245]);
     ( 19611862, [ 20031944; 19111760]);
     ( 20031944, [ 19611862]);
     ( 18081847, [ 18421909; 18261870; 17541744; 17511786; 17961846]);
     ( 18421909, [ 18081847; 18741823; 18921875; 18621829]);
     ( 18741823, [ 18421909; 18921875; 18621829; 19111760]);
     ( 18921875, [ 18421909; 18621829; 19111760; 18741823]);
     ( 18621829, [ 18421909; 18921875; 19111760; 18741823]);
     ( 19111760, [ 18921875; 19611862; 18621829; 18741823]);
     ( 17961846, [ 18261870; 18081847; 17541744; 17511786]);
     ( 18261870, [ 17961846; 18081847]);
     ( 17541744, [ 18081847; 17961846; 17211819; 17961751; 17121747]);
     ( 17511786, [ 18081847; 17961846; 17211819; 17121747]);
     ( 17211819, [ 17541744; 17511786; 16721655; 17121747]);
     ( 16721655, [ 17211819; 17121747]);
     ( 17121747, [ 17211819; 17511786; 16721655; 17541744]);
     ( 17961751, [ 17541744]);
     ( 15921650, [ 16391737]);
     ( 16391737, [ 15921650]);
     ( 14931553, [ 15191397]);
     ( 15191397, [ 14931553]);
     ( 14271337, [ 14731456]);
     ( 14731456, [ 14271337]);
     ( 12251209, [ 12491300]);
     ( 12491300, [ 12251209])]
  solve g |> printfn "%A"
  for c,o in solve g |> Option.get do
    Graph.cutNodes [c;o] g |> solve |> printfn "%A"
  let g =
    [(new Contestant(("Herman Grooten 2199/2276",0),(false,0)), [new Contestant(("Mart Nabuurs 2171/2268",0),(false,0))]);
     (new Contestant(("Mart Nabuurs 2171/2268",0),(false,0)), [new Contestant(("Herman Grooten 2199/2276",0),(false,0))]);
     (new Contestant(("Bram van den Berg 2245/2245",0),(false,0)), [new Contestant(("Frans Konings 2268/2230",0),(false,0))]);
     (new Contestant(("Frans Konings 2268/2230",0),(false,0)), [new Contestant(("Bram van den Berg 2245/2245",0),(false,0))]);
     (new Contestant(("Joost Op 't Hoog 1961/1862",0),(false,0)),
      [new Contestant(("Rick van Loy 2003/1944",0),(false,0)); new Contestant(("Arend-Jan Meerwijk 1911/1760",0),(false,0))]);
     (new Contestant(("Rick van Loy 2003/1944",0),(false,0)), [new Contestant(("Joost Op 't Hoog 1961/1862",0),(false,0))]);
     (new Contestant(("Rick Zegveld 1808/1847",0),(false,0)),
      [new Contestant(("Stefan Hess 1842/1909",0),(false,0)); new Contestant(("Lex Karstens 1826/1870",0),(false,0)); new Contestant(("Hennie Daniëls 1754/1744",0),(false,0));
       new Contestant(("Bas Rosheuvel 1751/1786",0),(false,0)); new Contestant(("Kienfong Lie Kwie 1796/1846",0),(false,0))]);
     (new Contestant(("Stefan Hess 1842/1909",0),(false,0)),
      [new Contestant(("Rick Zegveld 1808/1847",0),(false,0)); new Contestant(("Ron de Veen 1874/1823",0),(false,0)); new Contestant(("Loek Mostertman 1892/1875",0),(false,0));
       new Contestant(("Dick Straathof 1862/1829",0),(false,0))]);
     (new Contestant(("Ron de Veen 1874/1823",0),(false,0)),
      [new Contestant(("Stefan Hess 1842/1909",0),(false,0)); new Contestant(("Loek Mostertman 1892/1875",0),(false,0)); new Contestant(("Dick Straathof 1862/1829",0),(false,0));
       new Contestant(("Arend-Jan Meerwijk 1911/1760",0),(false,0))]);
     (new Contestant(("Loek Mostertman 1892/1875",0),(false,0)),
      [new Contestant(("Stefan Hess 1842/1909",0),(false,0)); new Contestant(("Dick Straathof 1862/1829",0),(false,0)); new Contestant(("Arend-Jan Meerwijk 1911/1760",0),(false,0));
       new Contestant(("Ron de Veen 1874/1823",0),(false,0))]);
     (new Contestant(("Dick Straathof 1862/1829",0),(false,0)),
      [new Contestant(("Stefan Hess 1842/1909",0),(false,0)); new Contestant(("Loek Mostertman 1892/1875",0),(false,0));
       new Contestant(("Arend-Jan Meerwijk 1911/1760",0),(false,0)); new Contestant(("Ron de Veen 1874/1823",0),(false,0))]);
     (new Contestant(("Arend-Jan Meerwijk 1911/1760",0),(false,0)),
      [new Contestant(("Loek Mostertman 1892/1875",0),(false,0)); new Contestant(("Joost Op 't Hoog 1961/1862",0),(false,0));
       new Contestant(("Dick Straathof 1862/1829",0),(false,0)); new Contestant(("Ron de Veen 1874/1823",0),(false,0))]);
     (new Contestant(("Kienfong Lie Kwie 1796/1846",0),(false,0)),
      [new Contestant(("Lex Karstens 1826/1870",0),(false,0)); new Contestant(("Rick Zegveld 1808/1847",0),(false,0)); new Contestant(("Hennie Daniëls 1754/1744",0),(false,0));
       new Contestant(("Bas Rosheuvel 1751/1786",0),(false,0))]);
     (new Contestant(("Lex Karstens 1826/1870",0),(false,0)), [new Contestant(("Kienfong Lie Kwie 1796/1846",0),(false,0)); new Contestant(("Rick Zegveld 1808/1847",0),(false,0))]);
     (new Contestant(("Hennie Daniëls 1754/1744",0),(false,0)),
      [new Contestant(("Rick Zegveld 1808/1847",0),(false,0)); new Contestant(("Kienfong Lie Kwie 1796/1846",0),(false,0));
       new Contestant(("Sebastiaan Dijksman 1721/1819",0),(false,0)); new Contestant(("Guus Vermeulen 1796/1751",0),(false,0));
       new Contestant(("Peter Gerlagh 1712/1747",0),(false,0))]);
     (new Contestant(("Bas Rosheuvel 1751/1786",0),(false,0)),
      [new Contestant(("Rick Zegveld 1808/1847",0),(false,0)); new Contestant(("Kienfong Lie Kwie 1796/1846",0),(false,0));
       new Contestant(("Sebastiaan Dijksman 1721/1819",0),(false,0)); new Contestant(("Peter Gerlagh 1712/1747",0),(false,0))]);
     (new Contestant(("Sebastiaan Dijksman 1721/1819",0),(false,0)),
      [new Contestant(("Hennie Daniëls 1754/1744",0),(false,0)); new Contestant(("Bas Rosheuvel 1751/1786",0),(false,0)); new Contestant(("Carel van Alphen 1672/1655",0),(false,0));
       new Contestant(("Peter Gerlagh 1712/1747",0),(false,0))]);
     (new Contestant(("Carel van Alphen 1672/1655",0),(false,0)),
      [new Contestant(("Sebastiaan Dijksman 1721/1819",0),(false,0)); new Contestant(("Peter Gerlagh 1712/1747",0),(false,0))]);
     (new Contestant(("Peter Gerlagh 1712/1747",0),(false,0)),
      [new Contestant(("Sebastiaan Dijksman 1721/1819",0),(false,0)); new Contestant(("Bas Rosheuvel 1751/1786",0),(false,0));
       new Contestant(("Carel van Alphen 1672/1655",0),(false,0)); new Contestant(("Hennie Daniëls 1754/1744",0),(false,0))]);
     (new Contestant(("Guus Vermeulen 1796/1751",0),(false,0)), [new Contestant(("Hennie Daniëls 1754/1744",0),(false,0))]);
     (new Contestant(("Maarten Heller 1592/1650",0),(false,0)), [new Contestant(("Maarten Werkhoven 1639/1737",0),(false,0))]);
     (new Contestant(("Maarten Werkhoven 1639/1737",0),(false,0)), [new Contestant(("Maarten Heller 1592/1650",0),(false,0))]);
     (new Contestant(("Guus van Heck 1493/1553",0),(false,0)), [new Contestant(("Maarten de Wit 1519/1397",0),(false,0))]);
     (new Contestant(("Maarten de Wit 1519/1397",0),(false,0)), [new Contestant(("Guus van Heck 1493/1553",0),(false,0))]);
     (new Contestant(("Marieke Sarton 1427/1337",0),(false,0)), [new Contestant(("Rob Leurs 1473/1456",0),(false,0))]);
     (new Contestant(("Rob Leurs 1473/1456",0),(false,0)), [new Contestant(("Marieke Sarton 1427/1337",0),(false,0))]);
     (new Contestant(("Paul Vermee 1225/1209",0),(false,0)), [new Contestant(("Thomas Tepe 1249/1300",0),(false,0))]);
     (new Contestant(("Thomas Tepe 1249/1300",0),(false,0)), [new Contestant(("Paul Vermee 1225/1209",0),(false,0))])]
    |> Graph.reduceForMatching

  g |> printfn "%A"
  solve g |> printfn "%A"

  for seed in 0..9 do
    randomGraph seed 2000 4000 |> solve
  nearGraph 2018 10 6 |> solve

  let doi white black =
    printf "%4d %4d" white black
    for f,c in [ info,'I'; loss,'L' ; draw,'D' ; win,'W' ; score,'S'] do
      f white black |> printf " %c:%4.2f" c
    printfn ""
  for b in 1000..100..2800 do
    for d in -200..50..200 do
      doi (b+d) b
  exit 0
#endif

  let sw = System.Diagnostics.Stopwatch.StartNew()
  for tournament, rounds in Tests.tournaments do
    let contestants = Tests.read rounds 0.0 tournament
    printfn "------------------------"
    printfn "%s - %d contestants" tournament contestants.Length
    for i in 1..rounds do
      printfn "------------------------"
      printfn "Results of round %d" i
      printfn "------------------------"
      let m = getPairing contestants
      m
      |> Seq.sortBy (fun (c,o) -> min c.Order o.Order)
      |> Seq.iter (fun (w,b) ->
         let r = Stats.simulate w.Elo b.Elo
         let d = match r with 0.0 -> "0-1" | 0.5 -> "rem" | _ -> "1-0"
         printfn "%O - %O : %s" w b d
         w.Play b false r
         b.Play w true (1.0 - r)
         )
      printfn "------------------------"
      printfn "Standings after round %d" i
      printfn "------------------------"
      printfn "%4s %3s %-20s %3s %4s" "TPR" "Pts" "Name" "Rnk" "Rat"
      printfn "------------------------"
      contestants
      |> Seq.sortBy (fun c -> c.Order)
      |> Seq.iter (fun c -> printfn "%4d %3.1f %-20s %4d %4d" c.Value c.Score c.Name c.Rank c.Elo)
      printfn "%O ms" sw.Elapsed

  0 // return an integer exit code
