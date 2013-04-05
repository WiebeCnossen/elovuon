namespace Elovuon

type Graph<'n when 'n : comparison> = ('n * ('n list)) list   // 'n list = direct connections
type Matching<'n when 'n : comparison> = ('n * 'n) list
type Forest<'n when 'n : comparison> = ('n * ('n list)) list  // 'n list = ancestors 
type Node<'n when 'n : comparison> =
  | Path of 'n list
  | Forest of Forest<'n> * 'n list
  | Nothing

module List =
  let rec skipWhile f = function [] -> [] | h::t as l -> if f h then skipWhile f t else l
  let takeWhile f =
    let rec inner a =
      function h::t when f h -> inner (h::a) t | _ -> List.rev a
    inner []
  let contains x = List.exists ((=) x)
  let rec intersects x y =
    match x, y with
    | [], _ | _, [] -> false
    | hx::tx, hy::ty ->
      if hx = hy then true else
      if hx < hy then intersects tx y else intersects x ty
  let rec merge x y =
    let rec inner a x y =
      match x, y with
      | [], l | l, [] -> List.rev a @ l
      | hx::tx, hy::ty when hx = hy -> inner (hx::a) tx ty
      | hx::tx, hy::ty when hx < hy -> inner (hx::a) tx y
      | hx::tx, hy::ty -> inner (hy::a) x ty
    inner [] x y

module Graph =
  let edge v w = if v < w then v, w else w, v
  let (|Opposing|) vertex = function opposing, v | v, opposing when v = vertex -> Some opposing | _ -> None
  let nodeEdges (v, l) = l |> List.map (edge v)
  let graphNodes graph = graph |> List.map fst
  let matchingNodes matching = matching |> List.collect (fun (v,w) -> [v;w])
  let graphEdges graph = graph |> List.collect (fun (v, l) -> l |> List.filter ((<) v) |> List.map (edge v))
  let edgeInGraph graph (v, w) =
    let rec inner =
      function
      | [] -> false
      | (x, l)::_ when x = v -> l |> List.contains w
      | (x, l)::_ when x = w -> l |> List.contains v
      | _::t -> inner t
    inner graph
  let fromEdges edges =
    edges
    |> Seq.collect (fun (v, w) -> [ v, w; w, v ])
    |> Seq.groupBy fst
    |> List.ofSeq
    |> List.map (fun (key, s) -> key, Seq.distinct s |> List.ofSeq |> List.map snd)
  let cutNodes vl graph =
    let vs = set vl
    graph
    |> List.choose (fun (v, l) ->
       if vs.Contains v then None
       else Some (v, l |> List.filter (vs.Contains >> not)))
  let leaveNodes vl graph =
    let vs = set vl
    graph
    |> List.choose (fun (v, l) ->
       if vs.Contains v |> not then None
       else Some (v, l |> List.filter vs.Contains))

  let rec private getLast = function h::[] -> h | h::t -> getLast t | _ -> failwith "empty list"
  let private tryGetLinked vertex graph = graph |> List.tryPick (fun (v, l) -> if v = vertex then Some l else None)
  let private getLinked vertex = tryGetLinked vertex >> Option.get
  let private getBlossom p1 p2 =
    let p1, p2 = List.rev p1, List.rev p2
    let rec inner l1 l2 =
      match l1, l2 with
      | _::h1::t1, _::h2::t2 when h1 = h2 -> inner (h1::t1) (h2::t2)
      | h1::t1, l2 -> h1, t1 @ l2
      | _ -> failwithf "invalid blossom %A %A" p1 p2
    inner p1 p2
  let private replaceBlossom l r node graph =
    let bs = getBlossom l r |> snd |> set
    let nodeNodes =
      graph
      |> List.choose (fun (v, l) ->
         if bs.Contains v then None
         elif l |> List.exists (fun w -> bs.Contains w) then Some v
         else None)
    let subGraph =
      graph
      |> List.choose (fun (v, l) ->
         if bs.Contains v then None else
         let lf = l |> List.filter (bs.Contains >> not)
         Some (v, if l |> List.exists (fun w -> bs.Contains w) then node :: lf else lf))
    if List.isEmpty nodeNodes then subGraph else (node, nodeNodes) :: subGraph

  let rec private findAugmentingPath<'n when 'n : comparison> (graph:Graph<'n>) (matching:Matching<'n>) =
    let rec inner forest =
      function
      | [] -> None
      | v::vertices ->
        let path = getLinked v forest
        let root = getLast path
        getLinked v graph
        |> List.fold (fun s w ->
           if List.head path = w then s else
           match s with
           | Path path -> Path path
           | Nothing -> Nothing
           | Forest (forest,vertices) ->
             match tryGetLinked w forest with
             | Some l when List.length l % 2 = 0 -> s
             | Some l when getLast l = root ->
               let root, blossom = getBlossom path l
               let dummy =
                 match (typeof<'n>).GetProperty("Zero") with
                 | null -> match 0 :> obj with :? 'n as n -> n | _ -> failwith "wrong type 1"
                 | prop -> match prop.GetValue(null) with :? 'n as n -> n | _ -> failwith "wrong type 2"
               let sg = replaceBlossom l path dummy graph
               let link = match List.skipWhile ((<>) root) l with _::w::t -> Some (edge dummy w) | _ -> None
               let sm =
                 let cut = matching |> List.filter (fun (v,w) -> (List.contains v blossom || List.contains w blossom) |> not)
                 match link with
                 | Some edge -> edge :: cut
                 | _ -> cut
               let expand h v =
                 if v <> dummy then [v] else
                 let w = blossom |> List.find (edge h >> edgeInGraph graph)
                 if w = root then [root] else
                 let i,o = if List.contains w l then l,path else path,l
                 let rec iedge p =
                   match p with
                   | v::x::t when v = w -> edge v x
                   | h::t -> iedge t
                   | [] -> failwith "iedge error"                       
                 (if List.contains (iedge i)  matching then
                    i |> List.skipWhile ((<>) w)
                  else
                    w :: (i |> List.takeWhile ((<>) w) |> List.rev) @ o)
                 |> List.takeWhile (set blossom).Contains
               let rec explode path =
                 match path with
                 | h::d::i::t when d = dummy ->
                   if Option.exists ((=) (edge h d)) link
                   then
                     h :: (d::i::t |> explode)
                   else
                     path |> List.collect (expand h)
                 | h::d::t when d = dummy ->
                   path |> List.collect (expand h)
                 | d::h::t when d = dummy ->
                   (explode [h;d] |> List.rev) @ t
                 | h::t -> h :: explode t
                 | [] -> []
               match findAugmentingPath sg sm |> Option.map explode with
               | Some path ->
                 if Seq.countBy id path |> Seq.exists (snd >> (<) 1) then failwithf "invalid path: %A" path
                 Path path
               | None -> Nothing
             | Some l -> List.rev l @ getLinked v forest |> Path
             | None ->
               match matching |> List.tryPick (function x,y | y,x when x = w -> Some y | _ -> None) with
               | None -> s
               | Some x -> Forest ((w, w::path) :: (x, x::w::path) :: forest, x :: vertices)) (Forest (forest, vertices))
        |> function
           | Path path -> Some path
           | Forest (forest, vertices) -> inner forest vertices
           | Nothing -> None 
       
    let exposed = (graphNodes graph |> set) - (matchingNodes matching |> set) |> Set.toList
    let forest = exposed |> List.map (fun v -> v, [v])
    inner forest exposed

  let private applyPath matching path =
    if List.length path % 2 = 1 then failwithf "Invalid path %A" path
    let matchingEdges, pathEdges = matching |> set, Seq.pairwise path |> Seq.map (fun (v,w) -> edge v w) |> set
    (matchingEdges - pathEdges) + (pathEdges - matchingEdges) |> Set.toList

  let initialMatching graph =
    let rec initial nodes edges =
      function
      | [] -> edges
      | (v,e)::t when Set.contains v nodes -> initial nodes edges t
      | (v,e)::t ->
        match e |> List.tryFind (fun w -> Set.contains w nodes |> not) with
        | Some x -> initial (nodes |> Set.add v |> Set.add x) (edge v x :: edges) t
        | None -> initial nodes edges t

    graph
    |> List.sortBy (snd >> List.length)
    |> initial Set.empty []

  let getChunks (graph:Graph<_>) =
    graph
    |> List.fold (fun chunks (v,e) ->
       let current = v :: e |> List.sort
       let classified =
         current :: chunks
         |> List.map (fun chunk ->
            List.intersects current chunk,
            chunk)
       let disconnected = classified |> List.filter (fst >> not) |> List.map snd
       let connected = classified |> List.filter fst |> List.map snd |> List.reduce List.merge
       connected :: disconnected) []

  let rec findPerfectMatching graph =
    let rec inner matching =
      if List.length matching = List.length graph / 2 then Some matching else
      match findAugmentingPath graph matching with
      | None -> None
      | Some path -> applyPath matching path |> inner

    if graph |> List.exists (snd >> List.isEmpty) then None else
    match graph |> List.tryPick (function v, [w] -> edge v w |> Some | _ -> None) with
    | Some (v, w) ->
      graph
      |> cutNodes [v; w]
      |> findPerfectMatching
      |> Option.map (fun sub -> edge v w :: sub)

    | None ->
      let chunks = getChunks graph
      if chunks |> List.sumBy (fun chunk -> List.length chunk / 2) < List.length graph / 2 then None else
      if List.length chunks > 1 then
        let rec sub matching =
          function
          | [] -> Some matching
          | chunk::t ->
            leaveNodes chunk graph
            |> findPerfectMatching
            |> Option.choose (fun next -> sub (next @ matching) t)
        List.sortBy List.length chunks
        |> sub []
      else
        let initial = initialMatching graph
        if List.length initial = List.length graph / 2 then Some initial else
        printf "?"
        inner initial
