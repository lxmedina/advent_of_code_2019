module AoC.D05

open FSharpPlus

type Word = Word of int with
    static member (+) (Word x, Word y) = Word (x + y)
    static member (*) (Word x, Word y) = Word (x * y)

type Addr = Addr of int with
    static member (-) (Addr x, n) = Addr (x - n)
    static member ofWord (Word n) = Addr n

type Prog = Map<Addr, Word>

type Inst =
    | Add
    | Mult
    | Read
    | Write
    | JumpNonzero
    | JumpZero
    | LessThan
    | Equals
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
    | 05 -> Some JumpNonzero
    | 06 -> Some JumpZero
    | 07 -> Some LessThan
    | 08 -> Some Equals
    | 99 -> Some Halt
    |  e -> failwithf "invalid instruction: %d" e

let arity = function
    | Add           -> 3
    | Mult          -> 3
    | Read          -> 1
    | Write         -> 1
    | JumpNonzero   -> 2
    | JumpZero      -> 2
    | LessThan      -> 3
    | Equals        -> 3
    | Halt          -> 0

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

let parse prog addr: Cmd * Addr =
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

let eval seed cmd prog stack (addr: Addr) =
    let get = getEnv prog
    let set = setEnv prog
    let noop () = prog, stack, addr
    match cmd with
    | Add, [p;q;out] ->
        set out (get p + get q), stack, addr
    | Mult, [p;q;out] ->
        set out (get p * get q), stack, addr
    | Read, [out] ->
        set out seed, stack, addr
    | Write, [p] ->
        // printfn "%A\t%A" p (get p)  // debugging... must be zero
        prog, (get p)::stack, addr
    | JumpNonzero, [p;q] ->
        match get p with
        | Word 0 -> noop ()
        | _ -> prog, stack, Addr.ofWord (get q)
    | JumpZero, [p;q] ->
        match get p with
        | Word 0 -> prog, stack, Addr.ofWord (get q)
        | _ -> noop ()
    | LessThan, [p;q;out] ->
        if get p < get q then Word 1 else Word 0
        |> set out, stack, addr
    | Equals, [p;q;out] ->
        if get p = get q then Word 1 else Word 0
        |> set out, stack, addr
    | Halt, [] ->
        Map.empty, stack, addr
    | e -> failwithf "invalid command: %A" e

let rec repl seed stack prog addr =
    let cmd, nxt = parse prog addr
    let prog', stack', nxt' = eval seed cmd prog stack nxt
    if Map.isEmpty prog' then
        head stack'
    else
        repl seed stack' prog' nxt'

let run seed (src: string seq) =
    let prog =
        src
        |> choose tryParse
        |> mapi (fun i w -> Addr i, Word w)
        |> Map.ofSeq
    let (Word result) = repl seed [] prog (Addr 0)
    result
