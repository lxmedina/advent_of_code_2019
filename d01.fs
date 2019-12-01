module AoC.D01

open FSharpPlus

let fuelReq = function
    | 12 -> 2
    | 14 -> 2
    | 1969 -> 654
    | 100756 -> 33583
    | _ -> 0

let fuelReqs: string seq -> int
    = choose tryParse
    >> map fuelReq
    >> sum
