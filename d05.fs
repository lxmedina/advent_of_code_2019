module AoC.D05

open FSharpPlus

type Word = Word of int with
    static member (+) (Word x, Word y) = Word (x + y)
    static member (*) (Word x, Word y) = Word (x * y)

type Addr = Addr of int

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

let addrOfWord (Word x) = Addr x

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

let getArg prog = function
    | Ref i -> prog |> Map.find i
    | Val n -> n

let setArg prog arg v =
    match arg with
    | Ref i -> prog |> Map.add i v
    | e -> failwithf "invalid write argument: %A" e

type Env = {
    prog: Prog
    stdin: Word list
    stdout: Word list
    nxt: Addr
}

let eval (env: Env) (cmd: Cmd): Env =
    let get = getArg env.prog
    let set = setArg env.prog
    let noop () = env
    match cmd with
    | Add, [p;q;o] -> { env with prog = set o (get p + get q) }
    | Mult, [p;q;o] -> { env with prog = set o (get p * get q) }
    | Read, [o] ->
        match env.stdin with
        | x::xs -> { env with prog = set o x; stdin = xs }
        | [] -> failwith "reading from empty input stream"
    | Write, [p] -> { env with stdout = get p :: env.stdout }
    | JumpNonzero, [p;q] ->
        match get p with
        | Word 0 -> noop ()
        | _ -> { env with nxt = addrOfWord (get q) }
    | JumpZero, [p;q] ->
        match get p with
        | Word 0 -> { env with nxt = addrOfWord (get q) }
        | _ -> noop ()
    | LessThan, [p;q;o] ->
        if get p < get q then Word 1 else Word 0
        |> fun x -> { env with prog = set o x }
    | Equals, [p;q;o] ->
        if get p = get q then Word 1 else Word 0
        |> fun x -> { env with prog = set o x }
    | Halt, [] -> { env with prog = Map.empty }
    | e -> failwithf "invalid command: %A" e

let rec repl env =
    if Map.isEmpty env.prog then
        head env.stdout
    else
        let cmd, nxt = parse env.prog env.nxt
        eval { env with nxt = nxt } cmd |> repl

let run input (src: string seq) =
    let prog =
        src
        |> choose tryParse
        |> mapi (fun i w -> Addr i, Word w)
        |> Map.ofSeq
    let (Word result) = repl { prog = prog; stdin = input; stdout = []; nxt = Addr 0 }
    result
