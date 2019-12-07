module AoC.D05

open FSharpPlus

type Addr = Addr of int

type Word = Word of int

type Prog = Map<Addr, Word>

type Inst =
    | Add
    | Mult
    | Read
    | Write
    | Halt

type Arg =
    | Ref of Addr
    | Val of Word

type Cmd = Inst * Arg list

let INSTSZ = 100

let (|Inst|_|) (Word x) =
    match x % INSTSZ with
    | 01 -> Some Add
    | 02 -> Some Mult
    | 03 -> Some Read
    | 04 -> Some Write
    | 99 -> Some Halt
    |  e -> failwithf "invalid instruction: %d" e

let arity = function
    | Add   -> 3
    | Mult  -> 3
    | Read  -> 1
    | Write -> 1
    | Halt  -> 0

let modes (Word n) =
    n / INSTSZ
    |> Seq.unfold (fun x ->
        Some (
            match x % 10 with
            | 0 -> Ref << Addr
            | 1 -> Val << Word
            | e -> failwithf "invalid argument mode: %d" e
            ,
            x / 10 ))

let parse addr prog: Cmd * Addr =
    let (Addr i) = addr
    match prog |> Map.find addr with
    | Inst inst as x ->
        let n = arity inst
        let words = seq {i+1 .. i+n} |>> (Addr >> flip Map.find prog)
        let args =
            (words, modes x)
            ||> Seq.map2 (fun (Word w) mode -> mode w)
            |> toList
        let cmd = (inst, args)
        let nxt = Addr (i + 1 + n)
        (cmd, nxt)
    | e -> failwithf "invalid input: %A" e 

let getEnv prog = function
    | Ref i -> prog |> Map.find i
    | Val n -> n

let setEnv prog arg v =
    match arg with
    | Ref i -> prog |> Map.add i v
    | e -> failwithf "invalid write argument: %A" e

let eval seed cmd prog stack =
    let get = getEnv prog
    let set = setEnv prog
    match cmd with
    | Add, [p;q;out] ->
        let (Word p', Word q') = (get p, get q) in Word(p'+ q')
        |> set out, stack
    | Mult, [p;q;out] ->
        let (Word p', Word q') = (get p, get q) in Word(p'* q')
        |> set out, stack
    | Read, [out] ->
        seed
        |> set out, stack
    | Write, [p] ->
        // printfn "%A\t%A" p (get p)  // debugging... must be zero
        prog, (get p)::stack
    | Halt, [] ->
        Map.empty, stack
    | e -> failwithf "invalid command: %A" e

let rec repl seed stack prog addr =
    let cmd, nxt = parse addr prog
    let prog', stack' = eval seed cmd prog stack
    if Map.isEmpty prog' then
        head stack'
    else
        repl seed stack' prog' nxt

let run (src: string seq) =
    let prog =
        src
        |> choose tryParse
        |> mapi (fun i w -> Addr i, Word w)
        |> Map.ofSeq
    let seed = Word 1
    let (Word result) = repl seed [] prog (Addr 0)
    result
