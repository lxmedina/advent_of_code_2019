module AoC.D04

open FSharpPlus

let digits =
    function
    | 0 -> None
    | x -> Some (x % 10, x / 10)
    |> List.unfold

let rules: int seq -> bool =
    Seq.pairwise
    >> fun xs' -> 
        Seq.exists (uncurry (=)) xs' && // two adjacent digits are the same
        Seq.forall (uncurry (>=)) xs'   // from left to right, the digits never decrease

let genPass (lo, hi) = seq {lo..hi} |> filter (digits >> rules)

let passCount = genPass >> Seq.length

let (|Int|_|): string -> int option = tryParse

let run: string seq -> int = 
    Seq.toList
    >> function
    | [Int lo; Int hi] -> passCount (lo, hi)
    | xs -> failwithf "invalid input: %A" xs
