module Aoc22.ActiveDay

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
    // |> text2tokens "x"
    // |> text2tokensStr ["abc";"def"]

let parse2lines (text:string) = 
    text
    |> text2lines 
    |> List.map parseLine

let solve1 (text:string) = 
    let inp = text |> parse2lines
    0
    
let solve2 (text:string) =
    let inp = text |> parse2lines
    0

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res1.ToString())


let finished = true
()