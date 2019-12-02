open FSharpPlus
open AoC.Repo


let usage () =
    printfn "\nusage:"
    printfn "   dotnet run -- <day><a|b> [-f <file>] [-c <file>] [<args>]"
    printfn "\nexamples :"
    printfn "   dotnet run -- 1a 12 14"
    printfn "   dotnet run -- 1a \"1969 100756\""
    printfn "   dotnet run -- 1a -f data/d01"
    printfn "   dotnet run -- 2a -c data/d02"
    printfn "\ntests:"
    printfn "   dotnet test [--filter d<day><a|b>]"
    printfn "   dotnet test --filter d01a"


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
