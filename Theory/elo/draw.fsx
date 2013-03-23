open System.IO

let games =
  File.ReadLines "elo.csv"
  |> Seq.skip 1
  |> Seq.map (fun line ->
     let fields = line.Split [|','|]
     int fields.[2], float fields.[5])

let doit n =
  games
  |> Seq.filter (fun (diff, _) -> n - diff |> abs |> (>=) 50)
  |> Seq.map (fun (diff, result) ->
     let diff, result = diff - n |> abs, 2.0 * (result - 0.5)
     let weight = 1.0 / (1.0 + float diff)
     weight * result, weight)
  |> Seq.reduce (fun (ar,aw) (br,bw) -> ar + br, aw + bw)
  |> fun (r,w) -> r / w

for n in -40..-30 do
  doit n |> printfn "%3d : %f" n