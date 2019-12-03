module AoC.D02

open FSharpPlus

type Addr = Addr of int

type Op<'a> =
    | Bin of ('a -> 'a -> 'a)
    | End

let (|OpCode|_|) = function
    | 1  -> Some (Bin (+))
    | 2  -> Some (Bin (*))
    | 99 -> Some End
    | _  -> None

let inline write (xs: 'a list) (Addr i, x: 'a) =
    xs.[0..i-1] @ [x] @ xs.[i+1..]

let inline compute op (Addr i0, Addr i1, Addr iO) src =
    write src (Addr iO, op src.[i0] src.[i1])

let rec eval strict (Addr i) (src: int list) =
    match src.[i..] with
    | OpCode (Bin op)::i0::i1::iO::_ ->
        src
        |> compute op (Addr i0, Addr i1, Addr iO)
        |> eval strict (Addr (i + 4))
    | OpCode End::_ -> src
    | x::_ -> if strict then failwithf "invalid int-code %d at index %d" x i else []
    | [] -> if strict then failwith "unterminated int-code program" else []

let exec strict = eval strict (Addr 0)

let parse: string seq -> int list = choose tryParse >> toList

let patch program (inputs: (Addr * 'a) seq) = fold write program inputs

let run strict program = patch program >> exec strict >> tryHead

let intcodeProg: string seq -> int list = parse >> exec true

let input (noun, verb) = seq {
    yield (Addr 1, noun)
    yield (Addr 2, verb) }

let intcode strict source pair = run strict (parse source) (input pair)

let intcode1202 source = intcode true source (12, 02) |> Option.get

let tune output source =
    uncurry Seq.allPairs
    >> map (fun pair -> pair, intcode false source pair)
    >> skipWhile (snd >> (<>) output)
    >> head
    >> fst

let tune99 output source =
    (seq {0 .. 99}, seq {0 .. 99})
    |> tune output source
    |> fun (x, y) -> 100 * x + y
