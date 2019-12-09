module AoC.D07

open FSharpPlus
open D05

let pipeline (env: Env): Word seq -> Word -> Word =
    map (fun x y -> repl { env with stdin = [x; y] })
    >> flip (fold (|>))

let rec insertions x = function
    | []            -> [[x]]
    | (y::ys) as l  -> (x::l)::(insertions x ys |> List.map (List.cons y))

let rec permutations = function
    | []    -> seq [ [] ]
    | x::xs -> permutations xs |> Seq.collect (insertions x)

let run src =
    let env = { prog = load src; stdin = []; stdout = []; nxt = Addr 0 }
    let seed = Seq.map Word
    let input = Word 0
    [0..4]
    |> permutations
    |> Seq.map (fun xs -> pipeline env (seed xs) input, xs)
    |> Seq.maxBy fst

let run' src = let (Word output, _) = run src in output
