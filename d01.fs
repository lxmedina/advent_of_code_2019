module AoC.D01

open FSharpPlus

let fuelReq x = x / 3 - 2

let fuelReqs: string seq -> int =
    choose tryParse
    >> map fuelReq
    >> sum

let fuelReqEx =
    fuelReq
    >> fun x -> if x < 0 then None else Some (x, x)
    |> Seq.unfold

let fuelReqsEx: string seq -> int =
    choose tryParse
    >> Seq.collect fuelReqEx
    >> sum
