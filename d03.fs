module AoC.D03

open FSharpPlus

let parseDir = function
    | 'U' -> fun (x, y) -> (x, y + 1)
    | 'D' -> fun (x, y) -> (x, y - 1)
    | 'L' -> fun (x, y) -> (x - 1, y)
    | 'R' -> fun (x, y) -> (x + 1, y)
    | x -> failwithf "invalid direction %c" x

let parseSegment (x: string) =
    parseDir x.[0]
    |> Seq.replicate (int x.[1..])

let parsePath =
    String.split [","]
    >> Seq.collect parseSegment

let renderPath =
    parsePath
    >> fold (fun acc f -> f (head acc)::acc) [(0,0)]

let intersect path0 path1 =
    (Set path0, Set path1)
    ||> Set.intersect 
    |> Set.remove (0,0)

let radius (x, y) = abs x + abs y 

let closestCrossing (p0, p1) = 
    intersect (renderPath p0) (renderPath p1)
    |> Set.map radius
    |> Set.minElement

let pathlength paths intersection =
    paths |> map (findIndex ((=) intersection)) |> sum

let shortestCrossing (p0, p1) = 
    let path0 = renderPath p0 |> rev
    let path1 = renderPath p1 |> rev
    intersect path0 path1
    |> Set.map (pathlength [path0; path1])
    |> Set.minElement

let run f = 
    Seq.toList
    >> function
    | [p0; p1] -> f (p0, p1)
    | xs -> failwithf "invalid input: %A" xs

let closestIntersection: string seq -> int = run closestCrossing

let shortestIntersection: string seq -> int = run shortestCrossing
