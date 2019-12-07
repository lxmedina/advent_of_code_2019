module AoC.D06

open FSharpPlus

let CENTER = "COM"

let rec orbits planets depth x = seq {
    match planets |> Map.tryFind x with
    | Some xs ->
        let depth' = depth + 1
        yield depth' * List.length xs
        yield! xs |> Seq.collect (orbits planets depth')
    | None -> yield 0
}

let push center planet system =
    system
    |> Map.tryFind center
    |> function
    | Some xs -> Map.add center (planet::xs) system
    | None -> Map.add center [planet] system

let parse src =
    src
    |> Seq.fold (fun acc x -> 
        match split [")"] x |> toList with
        | [center; planet] -> push center planet acc
        | e -> failwithf "invalid input: %A" e) Map.empty

let run src = (parse >> orbits) src 0 CENTER |> Seq.sum
