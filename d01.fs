module AoC.D01

open FSharpPlus

let fuelReq x = x / 3 - 2

let fuelReqs: string seq -> int =
    choose tryParse
    >> map fuelReq
    >> sum

let fuelReqEx =
    fun x ->
        let y = fuelReq x
        if y < 0 then None else Some(y, y)
    |> Seq.unfold

let fuelReqsEx: string seq -> int =
    choose tryParse
    >> Seq.collect fuelReqEx
    >> sum
