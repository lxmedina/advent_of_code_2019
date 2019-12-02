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

let compute op (i0, i1, iO) (src: int list) =
    src.[0..iO-1] @ [op src.[i0] src.[i1]] @ src.[iO+1..]

let rec evalIntcode i (src: int list) =
    match src.[i..] with
    | OpCode (Bin op)::i0::i1::iO::_ ->
        src
        |> compute op (i0, i1, iO)
        |> evalIntcode (i + 4)
    | OpCode End::_ -> src
    | x::_ -> failwithf "invalid int-code %d at index %d" x i
    | [] -> failwith "unterminated int-code program" 

let intcodeProg = evalIntcode 0

let intcodeProgram: string seq -> int list =
    choose tryParse >> toList >> intcodeProg

let intcodeProgram1202: string seq -> int =
    fun i x -> 
        match i with
        | 1 -> "12"
        | 2 -> "02"
        | _ -> x
    |> mapi
    >> intcodeProgram
    >> head
