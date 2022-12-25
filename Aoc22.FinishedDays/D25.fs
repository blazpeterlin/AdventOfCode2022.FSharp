module Aoc22.D25

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

//let input =  "input.txt" |> f2text

let parseLine (ln:string) =
    ln.ToCharArray() 
    |> List.ofSeq 
    |> List.map (function | '2' -> 2L | '1' -> 1L | '0' -> 0L | '-' -> -1L | '=' -> -2L)
    |> List.fold (
        fun num dgt ->
            num*5L + dgt
    ) 0L

let backwards (num0: int64) =
    let step (num:int64) acc =
        let lastDigit = num%5L |> fun x -> if x > 2L then x-5L else x
        let addedAcc = 
            match lastDigit with
            | 0L -> "0"
            | 1L -> "1"
            | 2L -> "2"
            | -1L -> "-"
            | -2L -> "="
        num/5L + (addedAcc |> function | "-" -> 1L | "=" -> 1L | _ -> 0L), addedAcc::acc

    let finalSt =
        {| Num=num0; Acc=[] |}
        |> Seq.unfold (
            fun obj ->
                if obj.Num=0L 
                then None 
                else
                    let (nextNum,nextAcc) = step obj.Num obj.Acc
                    let r = {|Num=nextNum;Acc=nextAcc|}
                    Some(r,r)
        )
        |> Seq.last
    let r2 = finalSt.Acc
    let r3 = r2
    let r4 = r3 |> List.reduce (+)

    r4



let parse2lines (text:string) = 
    text
    |> text2lines 
    |> List.map parseLine

let solve1 (text:string) = 
    let inp = text |> parse2lines
    let sm = inp |> List.sum
    let snafu = backwards sm
    snafu

//let res1 = input |> solve1

//ClipboardService.SetText(res1.ToString())


let finished = true
()