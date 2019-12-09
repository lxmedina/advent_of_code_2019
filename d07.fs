module AoC.D07

open FSharpPlus
open D05

let rec insertions x = function
    | []            -> [[x]]
    | (y::ys) as l  -> (x::l)::(insertions x ys |> List.map (List.cons y))

let rec permutations = function
    | []    -> seq [ [] ]
    | x::xs -> permutations xs |> Seq.collect (insertions x)

let pipeline (env: Env): Word seq -> Word -> Word =
    map (fun x y -> repl { env with stdin = [x; y] })
    >> flip (fold (|>))

let rec compute env ring =
    let cmd, nxt = parse env.prog env.nxt
    match eval { env with nxt = nxt } cmd with
    | Ok env' -> compute env' ring
    | Suspended env' ->
        match ring with
        | x::xs ->
            let ring' = xs @ [{ env' with stdout = [] }]
            compute { x with stdin = x.stdin @ env'.stdout } ring'
        | _ -> failwithf "suspension not supported"
    | Done result ->
        match ring with
        | [] -> result
        | x::xs -> compute { x with stdin = x.stdin @ env.stdout } xs

let loop (env: Env) seed input =
    let ring = Seq.tail seed |>> (fun x -> { env with stdin = [x] }) |> toList
    compute { env with stdin = [Seq.head seed; input] } ring

let run circuit settings src =
    let env = { prog = load src; stdin = []; stdout = []; nxt = Addr 0 }
    let seed = Seq.map Word
    let input = Word 0
    settings
    |> permutations
    |> Seq.map (fun xs -> circuit env (seed xs) input, xs)
    |> Seq.maxBy fst

let runA src = run pipeline [0..4] src

let runB src = run loop [5..9] src

let runA' src = let (Word output, _) = runA src in output

let runB' src = let (Word output, _) = runB src in output
