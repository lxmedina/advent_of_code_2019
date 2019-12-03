module AoC.Tests

open Xunit
open AoC.Repo
open FSharpPlus


let inline (==) (expected: ^a) (actual: ^a) = Assert.Equal< ^a> (expected, actual)

let inline (?->) (args, result) solver = result == solver (input [args])


[<Theory>]
[<InlineData("d01a", "-f", "d01", 3256599)>]
[<InlineData("d01b", "-f", "d01", 4882038)>]
[<InlineData("d02a", "-c", "d02", 4462686)>]
[<InlineData("d02c", "-c", "d02", 1202)>] // d02 inverse
let solution day fmt data result =
    result == problems.[day] (input [fmt; "data/" + data])


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


[<Theory>]
[<InlineData("1,0,0,0,99", "2,0,0,0,99")>]
[<InlineData("2,3,0,3,99", "2,3,0,6,99")>]
[<InlineData("2,4,4,5,99,0", "2,4,4,5,99,9801")>]
[<InlineData("1,1,1,4,99,5,6,0,99", "30,1,1,4,2,5,6,0,99")>]
let ``d02a - int-code program`` x y =
    (x, y) ?-> (D02.intcodeProg >> map string >> intercalate ",")

[<Theory>]
[<InlineData("1,99,99,0,99", 2, 0)>]
[<InlineData("2,99,99,0,99,7", 49, 505)>]
[<InlineData("1,99,99,4,99,5,6,0,99", 30, 0)>]
let ``d02b - int-code tuning`` source output input =
    (source, input) ?-> D02.tune99 (Some output)
