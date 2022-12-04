module Aoc22.D04

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
     |> text2tokens ",-"
     |> map int
     |> fun xs -> [xs[0],xs[1];xs[2],xs[3]]
    // |> text2tokensStr ["abc";"def"]

let parse2lines (text:string) = 
    text
    |> text2lines 
    // |> Input.list2groups ((=)"")
    |> List.map parseLine
    //|> List.concat

let solve1 (text:string) = 
    let inp = text |> parse2lines
    let r =
        inp
        //|> map (fun xs -> List.sortBy (fun lst -> lst[0],-lst[1]))
        |> filter (fun ((x1,y1)::[x2,y2]) -> x1<=x2 && y1 >= y2 || x2<=x1 && y2 >= y1)
        |> List.length
    r
    
let solve2 (text:string) =
    let inp = text |> parse2lines
    let r =
        inp
        //|> map (fun xs -> List.sortBy (fun lst -> lst[0],-lst[1]))
        |> filter (fun ((x1,y1)::[x2,y2]) -> x1<=y2 && y1 >= x2 || x2<=y1 && y2 >= x1)
        |> List.length
    r

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())


let finished = true
()