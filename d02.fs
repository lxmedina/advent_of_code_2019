module AoC.D02

open FSharpPlus

type Op =
    | Bin of (int -> int -> int)
    | End

let (|OpCode|_|) = function
    | 1  -> Some (Bin (+))
    | 2  -> Some (Bin (*))
    | 99 -> Some End
    | _  -> None

let inline writeAddr (xs: 'a list) (i: int, x: 'a) =
    xs.[0..i-1] @ [x] @ xs.[i+1..]

let computeBin op (i0, i1, iO) (src: int list) =
    writeAddr src (iO, op src.[i0] src.[i1])

let rec evalIntcode i (src: int list) =
    match src.[i..] with
    | OpCode (Bin op)::i0::i1::iO::_ ->
        src
        |> computeBin op (i0, i1, iO)
        |> evalIntcode (i + 4)
    | OpCode End::_ -> src
    | x::_ -> failwithf "invalid int-code %d at index %d" x i
    | [] -> failwith "unterminated int-code program" 

let intcodeProg = evalIntcode 0

let patch (diff: (int * 'a) list) (src: 'a list) =
    fold writeAddr src diff

let intcodeProgram diff: string seq -> int list =
    choose tryParse >> toList >> patch diff >> intcodeProg

let intcodeProgram1202: string seq -> int =
    intcodeProgram [(1, 12); (2, 2)] >> head

// let tune goal vars =
//     [for (i, xs) in vars]
