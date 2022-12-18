module Aoc22.D18

open Aoc22
open Input
open TextCopy
open System.Collections.Generic
open Aoc22.Operators
open Microsoft.FSharp.Core.Operators.Checked
open System.Text.RegularExpressions
open System
open FSharp.Text.RegexProvider
open FSharp.Text.RegexExtensions

let input =  "input.txt" |> f2text

let parseLine (ln:string) =
    ln
    |> text2tokens ","
    |> fun xs -> int xs[0],int xs[1], int xs[2]
    // |> text2tokensStr ["abc";"def"]

let parse2lines (text:string) = 
    text
    |> text2lines 
    // |> Input.list2groups ((=)"")
    |> List.map parseLine



let solve1 (text:string) = 
    let inp = text |> parse2lines
    let st = inp |> Set

    let moves = [
        -1, 0, 0
        1,  0, 0
        0,  1, 0
        0, -1, 0
        0, 0, -1
        0,  0, 1
    ]

    let r =
        List.allPairs inp moves
        |> List.map (fun (a,b) -> a +... b)
        |> List.filter (fun pos -> st.Contains pos |> not)
        |> List.length

    r
    
let solve2 (text:string) =
    let inp = text |> parse2lines
    let st = inp |> Set

    let moves = [
        -1, 0, 0
        1,  0, 0
        0,  1, 0
        0, -1, 0
        0, 0, -1
        0,  0, 1
    ]

    let getN pos =
        moves
        |> Seq.map (fun m -> pos +... m)
        |> Seq.filter (fun p -> st.Contains pos |> not)

    let neighbours =
        List.allPairs inp moves
        |> List.map (fun (a,b) -> a +... b)
        |> List.filter (fun pos -> st.Contains pos |> not)

    let minX = inp |> List.map (fun (x,y,z) -> x) |> List.min
    let minY = inp |> List.map (fun (x,y,z) -> y) |> List.min
    let minZ = inp |> List.map (fun (x,y,z) -> z) |> List.min

    let targetCoord = minX-1,minY-1,minZ-1

    let dictReachable = [targetCoord,true] |> Map |> Dictionary // mutable

    for n in neighbours do
        if dictReachable.ContainsKey(n) then 0|>ignore else
        let isOutside,vs = Common.DijkstraExistsAndGetVisited n getN ((=)targetCoord)
        for v in vs do
            dictReachable[v] <- isOutside

    let r = 
        neighbours
        |> List.filter(fun n -> 
            dictReachable[n]
        )
        |> List.length

    r

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())


let finished = true
()