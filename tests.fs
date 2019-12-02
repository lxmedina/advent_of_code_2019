module AoC.Tests

open Xunit
open AoC.Repo

let inline (==) (expected: ^a) (actual: ^a) = Assert.Equal< ^a> (expected, actual)

let inline private (?->) (args, result) solver = result == solver (input [args])


[<Theory>]
[<InlineData("d01a", "d01", 3256599)>]
[<InlineData("d01b", "d01", 4882038)>]
let solution day data result =
    result == problems.[day] (input ["-f"; "data/" + data])


[<Theory>]
[<InlineData("12", 2)>]
[<InlineData("14", 2)>]
[<InlineData("1969", 654)>]
[<InlineData("100756", 33583)>]
let ``d01a - total fuel requirements`` x y = (x, y) ?-> D01.fuelReqs

[<Theory>]
[<InlineData("12", 2)>]
[<InlineData("14", 2)>]
[<InlineData("1969", 966)>]
[<InlineData("100756", 50346)>]
let ``d01b - fuel requirements extended`` x y = (x, y) ?-> D01.fuelReqsEx
