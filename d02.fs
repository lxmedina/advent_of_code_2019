module AoC.D02

open FSharpPlus

type Addr = Addr of int

type Diff<'a> = (Addr * 'a) seq

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

let inline patch<'a> (diff: Diff<'a>) src = fold write src diff

let inline compute op (Addr i0, Addr i1, Addr iO) src =
    write src (Addr iO, op src.[i0] src.[i1])

let rec eval (Addr i) (src: int list) =
    match src.[i..] with
    | OpCode (Bin op)::i0::i1::iO::_ ->
        src
        |> compute op (Addr i0, Addr i1, Addr iO)
        |> eval (Addr (i + 4))
    | OpCode End::_ -> src
    | x::_ -> failwithf "invalid int-code %d at index %d" x i
    | [] -> failwith "unterminated int-code program" 

let exec = eval (Addr 0)

let parse: string seq -> int list = choose tryParse >> toList

let intcodeProgram: string seq -> int list = parse >> exec

let intcodeProgram1202: string seq -> int =
    parse
    >> patch [(Addr 1, 12); (Addr 2, 2)]
    >> exec
    >> head

// let tune (diffs: Diff seq) goal src =
//     diffs
//     |> skipWhile (fun x -> intcodeHead x src <> goal)
//     |> tryHead

// let tune99 idxs goal src =
//     Seq.un