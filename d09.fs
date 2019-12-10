module AoC.D09

open FSharpPlus
open D05

let rec repl env =
    let cmd, nxt = parse env.prog env.nxt
    match eval { env with nxt = nxt } cmd with
    | Done stdout -> stdout
    | Ok env' -> repl env'
    | Suspended _ -> failwith "suspension not supported in repl"

let run input src =
    { prog = load src; stdin = input; stdout = []; nxt = Addr 0L }
    |> repl |>> (fun (Word x) -> string x) |> intercalate ","
