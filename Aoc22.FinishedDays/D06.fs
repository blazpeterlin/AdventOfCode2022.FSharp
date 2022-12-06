module Aoc22.D06

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
    ln.ToCharArray() |> List.ofSeq
    // |> text2tokens "x"
    // |> text2tokensStr ["abc";"def"]

let parse2lines (text:string) = 
    text
    |> text2lines 
    // |> Input.list2groups ((=)"")
    |> List.map parseLine
    |> List.head

let solve1 (text:string) = 
    let inp = text |> parse2lines
    let r = 
        inp
        |> List.windowed 4
        |> List.indexed
        |> List.filter(fun (idx,chars) -> chars.Length = (chars |>List.distinct).Length)
        |> List.head
        |> fst
    r+4
    
let solve2 (text:string) =
    let inp = text |> parse2lines
    let r = 
        inp
        |> List.windowed 14
        |> List.indexed
        |> List.filter(fun (idx,chars) -> chars.Length = (chars |>List.distinct).Length)
        |> List.head
        |> fst
    r+14

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())


let finished = true
()