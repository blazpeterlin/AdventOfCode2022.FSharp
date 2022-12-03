module Aoc22.D03

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
    let x1 = ln.Substring(0, ln.Length/2)
    let x2 = ln.Substring(ln.Length/2,ln.Length/2)
    x1.ToCharArray() |> List.ofArray,x2.ToCharArray() |> List.ofArray
    // |> text2tokens "x"
    // |> text2tokensStr ["abc";"def"]

let parse2lines (text:string) = 
    text
    |> text2lines 
    // |> Input.list2groups ((=)"")
    |> List.map parseLine

let parse2lines2 (text:string) = 
    text
    |> text2lines 
    |> map (fun x -> x.ToCharArray() |> List.ofArray)

let findSame arrA arrB =
    arrA |> filter (fun ch -> arrB |> List.contains ch) |> List.head
    
let findSameLst arrA arrB =
    arrA |> filter (fun ch -> arrB |> List.contains ch)

let solve1 (text:string) = 
    let inp = text |> parse2lines

    let r =
        inp
        |> map (fun (x,y) -> findSame x y)
        |> map (fun ch -> if Char.IsLower(ch) then (int ch) - (int 'a') + 1 else (int ch) - (int 'A') + 27)
        |> List.sum
    r
    
let solve2 (text:string) =
    let inp = text |> parse2lines2
    let r = 
        inp
        |> List.chunkBySize 3
        |> map (fun (a::b::[c]) -> findSameLst a b |> findSameLst c |> List.head)
        |> map (fun ch -> if Char.IsLower(ch) then (int ch) - (int 'a') + 1 else (int ch) - (int 'A') + 27)
        |> List.sum
    r

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())


let finished = true
()