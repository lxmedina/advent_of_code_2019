module AoC.D05

open FSharpPlus

type Word = Word of int64 with
    static member (+) (Word x, Word y) = Word (x + y)
    static member (*) (Word x, Word y) = Word (x * y)

type Addr = Addr of int64 with
    static member (+) (Addr i, Addr j) = Addr (i + j)
    static member (-) (Addr i, n) = Addr (i - n)

type Prog = { memspace: Map<Addr, Word>; relbase: Addr }

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
    | Rel of Addr

type Cmd = Inst * Arg list

type Env = {
    prog: Prog
    stdin: Word list
    stdout: Word list
    nxt: Addr
}

type EvalResult =
    | Ok of Env
    | Done of Word
    | Suspended of Env

let INSTSZ = 100L

let (|Inst|_|) (Word x) =
    match x % INSTSZ with
    | 01L -> Some Add
    | 02L -> Some Mult
    | 03L -> Some Read
    | 04L -> Some Write
    | 05L -> Some JumpNonzero
    | 06L -> Some JumpZero
    | 07L -> Some LessThan
    | 08L -> Some Equals
    | 99L -> Some Halt
    |   e -> failwithf "invalid instruction: %d" e

let arity = function
    | Add           -> 3L
    | Mult          -> 3L
    | Read          -> 1L
    | Write         -> 1L
    | JumpNonzero   -> 2L
    | JumpZero      -> 2L
    | LessThan      -> 3L
    | Equals        -> 3L
    | Halt          -> 0L

let modes (Word n) =
    n / INSTSZ
    |> Seq.unfold (fun x ->
        Some (
            match x % 10L with
            | 0L -> Ref << Addr
            | 1L -> Val << Word
            | 2L -> Rel << Addr
            |  e -> failwithf "invalid argument mode: %d" e
            ,
            x / 10L ))

let addrOfWord (Word x) = Addr x

let parse prog addr: Cmd * Addr =
    let (Addr i) = addr
    match prog.memspace |> Map.find addr with
    | Inst inst as x ->
        let n = arity inst
        let words = seq {i+1L .. i+n} |>> (Addr >> flip Map.find prog.memspace)
        let args =
            (words, modes x)
            ||> Seq.map2 (fun (Word w) mode -> mode w)
            |> toList
        let cmd = (inst, args)
        let nxt = Addr (i + 1L + n)
        (cmd, nxt)
    | e -> failwithf "invalid input: %A" e 

let rec getArg prog = function
    | Ref i -> prog.memspace |> Map.tryFind i |> Option.defaultValue (Word 0L)
    | Val n -> n
    | Rel j -> Ref (prog.relbase + j) |> getArg prog

let rec setArg prog arg v =
    match arg with
    | Ref i -> { prog with memspace = prog.memspace |> Map.add i v }
    | Rel j -> Ref (prog.relbase + j) |> flip (setArg prog) v
    | e -> failwithf "invalid write argument: %A" e

let eval (env: Env) (cmd: Cmd): EvalResult =
    let get = getArg env.prog
    let set = setArg env.prog
    let noop () = Ok env
    match cmd with
    | Add, [p;q;o] -> Ok { env with prog = set o (get p + get q) }
    | Mult, [p;q;o] -> Ok { env with prog = set o (get p * get q) }
    | Read, [o] ->
        match env.stdin with
        | x::xs -> Ok { env with prog = set o x; stdin = xs }
        | [] -> Suspended { env with nxt = env.nxt - 2L }
    | Write, [p] -> Ok { env with stdout = get p :: env.stdout }
    | JumpNonzero, [p;q] ->
        match get p with
        | Word 0L -> noop ()
        | _ -> Ok { env with nxt = addrOfWord (get q) }
    | JumpZero, [p;q] ->
        match get p with
        | Word 0L -> Ok { env with nxt = addrOfWord (get q) }
        | _ -> noop ()
    | LessThan, [p;q;o] ->
        if get p < get q then Word 1L else Word 0L
        |> fun x -> Ok { env with prog = set o x }
    | Equals, [p;q;o] ->
        if get p = get q then Word 1L else Word 0L
        |> fun x -> Ok { env with prog = set o x }
    | Halt, [] -> Done (head env.stdout)
    | e -> failwithf "invalid command: %A" e

let rec repl env =
    let cmd, nxt = parse env.prog env.nxt
    match eval { env with nxt = nxt } cmd with
    | Done result -> result
    | Ok env' -> repl env'
    | Suspended _ -> failwith "suspension not supported in repl"

let load: string seq -> Prog =
    choose tryParse
    >> mapi (fun i w -> Addr (int64 i), Word w)
    >> Map.ofSeq
    >> fun m -> { memspace = m; relbase = Addr 0L }

let run input src =
    let env = { prog = load src; stdin = input; stdout = []; nxt = Addr 0L }
    let (Word result) = repl env
    int result
