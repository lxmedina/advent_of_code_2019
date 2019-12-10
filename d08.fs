module AoC.D08

open FSharpPlus

let countif x = Seq.filter ((=) x) >> Seq.length

let chunkcheck sz =
    Seq.chunkBySize sz
    >> Seq.minBy (countif '0')
    >> fun x -> countif '1' x * countif '2' x

let runA size: string seq -> int = Seq.collect id >> chunkcheck size

let TRANSP = '2'

let merge = Seq.map2 (fun x y -> if x = TRANSP then y else x)

let empty sz = Seq.init sz (fun _ -> TRANSP)

let render w =
    map (function
        | '0' -> ' '
        | '1' -> '#'
        | _   -> '?')
    >> Seq.chunkBySize w
    >> map String.ofArray
    >> String.concat "\n"
    >> sprintf "\n%s"

let runB w h: string seq -> string =
    Seq.collect id 
    >> Seq.chunkBySize (w * h)
    >> Seq.fold merge (empty (w * h))
    >> render w
