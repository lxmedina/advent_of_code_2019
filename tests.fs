module AoC.Tests

open Xunit
open AoC.Repo
open FSharpPlus


let inline (==) (expected: ^a) (actual: ^a) = Assert.Equal< ^a> (expected, actual)

let inline (?->) (args, result) solver = result == solver (input [args])


[<Theory>]
[<InlineData("d01a", "-f", "data/d01", 3256599)>]
[<InlineData("d01b", "-f", "data/d01", 4882038)>]
[<InlineData("d02a", "-c", "data/d02", 4462686)>]
[<InlineData("d02b", "-c", "data/d02", 5936)>]
[<InlineData("d02c", "-c", "data/d02", 1202)>] // d02 inverse
[<InlineData("d03a", "-f", "data/d03", 308)>]
[<InlineData("d03b", "-f", "data/d03", 12934)>]
[<InlineData("d04a", "264360", "746325", 945)>]
[<InlineData("d04b", "264360", "746325", 617)>]
[<InlineData("d05a", "-c", "data/d05", 15426686)>]
let solution day fmt data result =
    result == problems.[day] (input [fmt; data])


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


[<Theory>]
[<InlineData("R8,U5,L5,D3", "U7,R6,D4,L4", 6)>]
[<InlineData("R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83", 159)>]
[<InlineData("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7", 135)>]
let ``d03a - closest intersection`` x0 x1 y =
    ("\n" + x0 + "\n" + x1, y) ?-> D03.closestIntersection


[<Theory>]
[<InlineData("R8,U5,L5,D3", "U7,R6,D4,L4", 30)>]
[<InlineData("R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83", 610)>]
[<InlineData("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7", 410)>]
let ``d03b - shortest intersection`` x0 x1 y =
    ("\n" + x0 + "\n" + x1, y) ?-> D03.shortestIntersection


[<Theory>]
[<InlineData(111111, 111111, 1)>]
[<InlineData(111111, 111112, 2)>]
[<InlineData(111120, 111121, 0)>]
[<InlineData(223450, 223450, 0)>]
[<InlineData(123789, 123789, 0)>]
[<InlineData(123788, 123800, 2)>]
let ``d04a - possible passcodes / dupes`` lo hi count =
    (lo + " " + hi, count) ?-> D04.run D04.rulesA


[<Theory>]
[<InlineData(112233, 112233, 1)>]
[<InlineData(123444, 123444, 0)>]
[<InlineData(111120, 111122, 1)>]
[<InlineData(123888, 123900, 2)>]
[<InlineData(100, 200, 16)>]
let ``d04b - possible passcodes / pairs`` lo hi count =
    (lo + " " + hi, count) ?-> D04.run D04.rulesB
