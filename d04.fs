module AoC.D04

open FSharpPlus

let digits =
    function
    | 0 -> None
    | x -> Some (x % 10, x / 10)
    |> List.unfold
    >> List.rev

let rulesA: int seq -> bool =
    Seq.pairwise
    >> fun xs' -> 
        Seq.exists (uncurry (=)) xs' && // two adjacent digits are the same
        Seq.forall (uncurry (<=)) xs'   // the digits never decrease

let rulesB: int seq -> bool =
    Seq.fold (fun (mono, pair, grp, w) x ->  // optimization pending ¯\_(ツ)_/¯
        let (grp', pair') = if x = w then (grp + 1, false) else (1, grp = 2)
        (mono && x >= w, pair || pair', grp', x)
    ) (true, false, 0, -1)
    >> fun (m, p, g, _) -> m && (p || g = 2)

let genPass rules (lo, hi) = seq {lo..hi} |> filter (digits >> rules)

let passCount rules = genPass rules >> Seq.length

let (|Int|_|): string -> int option = tryParse

let run rules: string seq -> int = 
    Seq.toList
    >> function
    | [Int lo; Int hi] -> passCount rules (lo, hi)
    | xs -> failwithf "invalid input: %A" xs
