module Aoc22.D01

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
    if ln = "" then None
    else Some (ln |> int)
    // |> text2tokens "x"
    // |> text2tokensStr ["abc";"def"]

let parse2lines (text:string) = 
    text
    |> text2lines 
    |> List.map parseLine

let solve1 (text:string) = 
    let inp = text |> parse2lines
    let grps : int list list = 
          inp 
          |> List.fold (fun (x::y) item -> if item = None then []::x::y else ((item.Value)::x)::y) [[]]

    let max = grps |> map (List.sum) |> List.max
    max
    
let solve2 (text:string) =
    let inp = text |> parse2lines
    let grps : int list list = 
          inp 
          |> List.fold (fun (x::y) item -> if item = None then []::x::y else ((item.Value)::x)::y) [[]]

    let max3 = grps |> map (List.sum) |> List.sortDescending |> List.take 3 |> List.sum
    max3

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())


let finished = true
()