module Aoc22.D02

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
    |> text2tokens " "
    |> fun xs -> xs[0][0],xs[1][0]
    // |> text2tokensStr ["abc";"def"]

let parse2lines (text:string) = 
    text
    |> text2lines 
    // |> Input.list2groups ((=)"")
    |> List.map parseLine

type Move = Rock | Paper | Scissors
type Outcome = Win | Loss | Draw

let score a b =
    let hisMove = match a with | 'A' -> Rock | 'B' -> Paper | 'C' -> Scissors
    let yourMove = match b with | 'X' -> Rock | 'Y' -> Paper | 'Z' -> Scissors

    let yourScore = match yourMove with | Rock -> 1 | Paper -> 2 | _ ->3
    let outcome = 
        match hisMove, yourMove with 
        | Rock, Paper | Paper, Scissors | Scissors, Rock -> Win
        | Paper, Rock | Scissors, Paper | Rock, Scissors -> Loss
        | _ -> Draw
    let outcomeScore = match outcome with | Win -> 6 | Draw -> 3 | Loss -> 0
    yourScore + outcomeScore

let score2 a b =
    let hisMove = match a with | 'A' -> Rock | 'B' -> Paper | 'C' -> Scissors
    let outcome = match b with | 'X' -> Loss | 'Y' -> Draw | 'Z' -> Win

    let yourMove = 
        match hisMove, outcome with 
        | Rock, Win | Paper, Draw | Scissors, Loss -> Paper
        | Paper, Win | Scissors, Draw | Rock, Loss -> Scissors
        | _ -> Rock
    let yourScore = match yourMove with | Rock -> 1 | Paper -> 2 | _ ->3
    let outcomeScore = match outcome with | Win -> 6 | Draw -> 3 | Loss -> 0
    yourScore + outcomeScore


let solve1 (text:string) = 
    let inp = text |> parse2lines
    let score = inp |> map (fun (a,b) -> score a b) |> List.sum
    score
    
let solve2 (text:string) =
    let inp = text |> parse2lines
    let score = inp |> map (fun (a,b) -> score2 a b) |> List.sum
    score

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())


let finished = true
()