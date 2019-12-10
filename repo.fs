module AoC.Repo

open FSharpPlus
open System.IO


let (~%) f = sprintf "%A" << f


let problems = Map<string, string seq -> _> [
    "d01a", % D01.fuelReqs
    "d01b", % D01.fuelReqsEx
    "d02a", % D02.intcode1202
    "d02b", % D02.tune99 (Some 19690720)
    "d02c", % D02.tune99 (Some 4462686) // d02 inverse
    "d03a", % D03.closestIntersection
    "d03b", % D03.shortestIntersection
    "d04a", % D04.run D04.rulesA
    "d04b", % D04.run D04.rulesB
    "d05a", % D05.run [D05.Word 1L]
    "d05b", % D05.run [D05.Word 5L]
    "d06a", % D06.totalOrbits
    "d06b", % D06.orbitalTransfers
    "d07a", % D07.runA'
    "d07b", % D07.runB'
    "d08a", % D08.runA (25 * 6)
]


let (|Problem|_|) =
    String.toLower
    >> sprintf "d%3s"
    >> replace " " "0"
    >> flip Map.tryFind problems


let input = function
    | "-f" :: [path] -> File.ReadLines path
    | "-c" :: [path] -> File.ReadAllText path |> String.split [","]
    | [x] when x.Length > 0 && x.[0] = '\n' -> x.[1..] |> String.split ["\n"]
    | xs -> (intercalate " " xs) |> String.split [" "; ","]
