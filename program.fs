open FSharpPlus
open AoC.Repo


let usage () =
    printfn "usage    : dotnet run -- <day><a|b> [-f <file>] [<args>]"
    printfn "examples :"
    printfn "           dotnet run -- 1a 12 14"
    printfn "           dotnet run -- 1a \"1969 100756\""
    printfn "           dotnet run -- 1a -f data/d01"
    printfn "tests    : "
    printfn "           dotnet test [--filter d<dd><a|a>]"
    printfn "           dotnet test --filter d01a"


let solve = function
    | (Problem solver)::xs ->
        (input >> solver >> printfn ">> result: %s") xs
    | xs ->
        printfn "invalid arguments: %A" (intercalate " " xs)
        usage ()


[<EntryPoint>]
let main argv =
    solve (List.ofArray argv)
    0
