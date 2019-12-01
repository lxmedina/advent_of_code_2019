module AoC.Repo

open FSharpPlus
open System.IO


let (~%) f = sprintf "%A" << f


let problems = Map<string, string seq -> _> [
    "d01a", % D01.fuelReqs
]


let (|Problem|_|) =
    String.toLower
    >> sprintf "d%3s"
    >> replace " " "0"
    >> flip Map.tryFind problems


let input = function
    | "-f" :: [path] -> File.ReadLines path
    | [x] when x.Length > 0 && x.[0] = '\n' -> x.[1..] |> String.split ["\n"]
    | xs -> (intercalate " " xs) |> String.split [" "; ","]
