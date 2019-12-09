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

let planets src =
    src
    |> Seq.fold (fun acc x -> 
        match split [")"] x |> toList with
        | [center; planet] -> push center planet acc
        | e -> failwithf "invalid input: %A" e) Map.empty

let totalOrbits src = (planets >> orbits) src 0 CENTER |> Seq.sum

let links src =
    src
    |> Seq.fold(fun acc x ->
        match split [")"] x |> toList with
        | [center; planet] -> acc |> Map.add planet center
        | e -> failwithf "invalid input: %A" e) Map.empty

let rec traverse lnks from = seq {
    match Map.tryFind from lnks with
    | Some center ->
        yield center
        yield! traverse lnks center
    | None -> ()
}

let orbitalTransfers src =
    let lnks = links src
    let path = traverse lnks >> set
    let you = path "YOU"
    let san = path "SAN"
    (Set.union you san) - (Set.intersect you san)
    |> Set.count
