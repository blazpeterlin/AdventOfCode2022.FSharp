module Aoc22.D10

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
    if ln = "noop" then (1,0) else
    ln
    |> text2tokens " "
    |> fun xs -> (2, int xs[1])
    // |> text2tokensStr ["abc";"def"]

let parse2lines (text:string) = 
    text
    |> text2lines 
    // |> Input.list2groups ((=)"")
    |> List.map parseLine

let solve1 (text:string) = 
    let inp = text |> parse2lines

    let x0 = (0,1)
    let valsAfterCycles =
        inp
        |> List.scan(fun (time,x) (dt,dx) -> (time+dt,x+dx)
        ) x0

    let ourCycles = [20;60;100;140;180;220]
    let ourCycleVals = 
        ourCycles
        |> map (fun t -> t)
        |> map (fun c -> valsAfterCycles |> List.filter (fun (t,x) -> t < c) |> List.sortBy fst |> List.last |> snd |> fun x -> c,x)
        
    let rs = ourCycleVals |> List.map (fun (t,x)-> (t)*x)
    let r = rs |>List.sum

    r
    
let solve2 (text:string) =
    let inp = text |> parse2lines

    let x0 = (0,1)
    let valsAfterCycles =
        inp
        |> List.scan(fun (time,x) (dt,dx) -> (time+dt,x+dx)
        ) x0

    let ourCycles = [0..40*6-1]
    let screenPosRegVal = 
        ourCycles
        |> map (fun t -> t)
        |> map (fun c -> valsAfterCycles |> List.filter (fun (t,x) -> t <= c) |> List.sortBy fst |> List.last |> snd |> fun x -> c%40,x)
    let screenLines =
        screenPosRegVal
        |> map (fun (pos,v) -> if abs(pos-v)<=1 then "█" else " ")
        |> List.chunkBySize 40
        |> map (fun chnk -> chnk |> List.reduce (+))

    for ln in screenLines do
        Console.WriteLine(ln)

    0



let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res1.ToString())


let finished = true
()