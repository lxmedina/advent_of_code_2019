module AoC.D01

open FSharpPlus

let fuelReq x = x / 3 - 2

let fuelReqs: string seq -> int
    = choose tryParse
    >> map fuelReq
    >> sum
