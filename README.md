# Advent of Code 2019

Some F# for the [Advent of Code 2019](https://adventofcode.com/2019)

### Build

.NET Core 2.2 and ...

```
$ git clone ...
$ cd ...
$ dotnet build
```

### Usage

```
# usage
    dotnet run -- <day><a|b> [<-f|-c> <file>] [<args>]
        -f : lines as input
        -c : comma separated
# examples
    dotnet run -- 1a -f data/d01
    dotnet run -- 2a -c data/d02
    dotnet run -- 3b -f data/d03
    dotnet run -- 1a 12 14
    dotnet run -- 2a 1,0,0,0,2,0,0,0,1,0,0,0,99
    dotnet run -- 3b $'\nR8,U5,L5,D3 \nU7,R6,D4,L4'
    dotnet run -- 4b 264360 746325
# tests
    dotnet test [--filter D<dd><A|B>]
    dotnet test --filter d01a
    dotnet test --filter solution
```
