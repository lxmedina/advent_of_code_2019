module AoC.D10

open FSharpPlus
open System

let points src = 
    src
    |> Seq.mapi (fun yi xs ->
        xs
        |> Seq.mapi (fun xi x ->
            if x = '#' then Some (xi, yi) else None))
    |> Seq.concat
    |> Seq.choose id
    |> Seq.cache

let slope (xi, yi) (xj, yj) =
    if xj <> xi then
        sign (xj - xi),
        Some (decimal (yj - yi) / decimal (xj - xi))
    else
        sign (yj - yi),
        None

let bestloc (src: string seq) =
    let ps = points src
    ps
    |> Seq.map (fun p ->
        ps
        |> Seq.except [p]
        |>> slope p
        |> Set.ofSeq
        |> tuple2 p)
    |> Seq.maxBy (snd >> Set.count)
    |> fun (p, qs) -> (p, Set.count qs)

let clockwise (xi, yi) (xj, yj) =
    let ``pi/2`` = Math.PI / 2.
    let at2 (* -π .. π *) = atan2 (float (yi - yj)) (float (xj - xi))
    if at2 <= ``pi/2`` && at2 >= 0. then
        ``pi/2`` - at2
    elif at2 < 0. then
        ``pi/2`` + abs(at2)
    else
        2. * Math.PI - (at2 - ``pi/2``)

let distance (xi, yi) (xj, yj) =
    pown (xj - xi) 2 + pown (yj - yi) 2 |> float |> sqrt 

let nthdead selection (src: string seq) =
    let ps = points src
    ps
    |> Seq.map (fun p ->
        ps
        |> Seq.except [p]
        |> Seq.groupBy (clockwise p)
        |> tuple2 p)
    |> Seq.maxBy (snd >> Seq.length)
    |> fun (loc, lines) ->
        lines
        |> Seq.collect (fun (angle, qs) ->
            qs
            |> Seq.sortBy (distance loc)
            |> Seq.indexed
            |> Seq.map (fun (i, q) -> (angle + 2. * Math.PI * (float i), q)))
        |> Seq.sortBy (fun (angle', _) -> angle')
        |> Seq.indexed
        |> Seq.map (fun (i, (_, (x, y))) -> i + 1, (x, y), 100 * x + y)
        |> Seq.filter (fun (i, _, _) -> selection |> List.contains i)
        |> Seq.toList

let debug5 src = nthdead [1;2;3;10;20;50;100;199;200;201;299] src
