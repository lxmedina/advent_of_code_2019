module AoC.D13

open FSharpPlus
open D05

let runA src =
    { prog = load src; stdin = []; stdout = []; nxt = Addr 0L }
    |> D09.repl
    |> rev
    |> Seq.chunkBySize 3
    |> filter (function
        | [| _; _; Word 2L |] -> true
        | _ -> false
    )
    |> length
