module AoC.D08

open FSharpPlus

let countif x = Seq.filter ((=) x) >> Seq.length

let chunkcheck sz =
    Seq.chunkBySize sz
    >> Seq.minBy (countif '0')
    >> fun x -> countif '1' x * countif '2' x

let runA size: string seq -> int = Seq.collect id >> chunkcheck size
