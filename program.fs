open FSharpPlus
open AoC.Repo


let usage () =
    printfn "\nusage:"
    printfn "   dotnet run -- <day><a|b> [<-f|-c> <file>] [-c <file>] [<args>]"
    printfn "\nexamples:"
    printfn "   dotnet run -- 1a 12 14"
    printfn "   dotnet run -- 2a -c data/d02"
    printfn "   dotnet run -- 3b -f data/d03"
    printfn "\ntests:"
    printfn "   dotnet test [--filter d<day>[a|b]]"
    printfn "   dotnet test --filter d01a"
    printfn "   dotnet test --filter solution\n"


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
