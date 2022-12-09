module Aoc22.D09

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
    |> fun xs -> xs[0][0],int xs[1]
    // |> text2tokensStr ["abc";"def"]

let parse2lines (text:string) = 
    text
    |> text2lines 
    // |> Input.list2groups ((=)"")
    |> List.map parseLine

let stepH (hx0,hy0) (dir,num) =
    let r =
        [1..num]
        |> List.scan (fun (hx,hy) _ ->
                            let delta = match dir with | 'L' -> (-1,0) | 'R' -> (+1,0) | 'D' -> (0,+1) | 'U' -> (0, -1)
                            (hx,hy)+..delta
        ) (hx0,hy0)
    r

let tailFollowH (tx0:int,ty0:int) (hx:int,hy:int) : (int*int) =
    if (tx0,ty0)=(hx,hy) then (hx,hy) else
    let dx = hx-tx0
    let dy = hy-ty0

    let possibleTX1,possibleTY1 =
        if abs(dx)=1 && abs(dy)=1 then (tx0,ty0) else
        if abs(dx)>=1 && abs(dy) >= 1 then (tx0,ty0) +.. (dx/abs(dx), dy/abs(dy)) else
        if dx <> 0 then (tx0,ty0) +.. (dx/abs(dx), 0)
        else (tx0,ty0) +.. (0, dy/abs(dy))
    if (possibleTX1,possibleTY1) = (hx,hy) then (tx0,ty0) else (possibleTX1,possibleTY1)

let solve1 (text:string) = 
    let inp = text |> parse2lines
    let st0h = (0,0)
    let allStepsH = 
        inp
        |> List.scan (
            fun (lstH) (dir,num) ->
                let hx,hy = lstH |> List.last
                stepH (hx,hy) (dir,num)
        ) [st0h]
        |> List.concat

    let st0t = (0,0)

    let allStepsT =
        allStepsH
        |> List.scan (
            fun (tx,ty) (hx,hy) ->
                let nextT = tailFollowH (tx,ty) (hx,hy)
                nextT
        ) st0t

    let r = allStepsT |> List.distinct |> List.length

    r
    
let solve2 (text:string) =
    let inp = text |> parse2lines
    let st0h = (0,0)
    let allStepsH = 
        inp
        |> List.scan (
            fun (lstH) (dir,num) ->
                let hx,hy = lstH |> List.last
                stepH (hx,hy) (dir,num)
        ) [st0h]
        |> List.concat


    let generateTailGen steps =
        let st0t = (0,0)
        steps
        |> List.scan (
            fun (tx,ty) (hx,hy) ->
                let nextT = tailFollowH (tx,ty) (hx,hy)
                nextT
        ) st0t

    let finalTailSteps =
        [1..9]
        |> List.fold (
            fun innerSteps i ->
                generateTailGen innerSteps
        ) allStepsH

    let r = finalTailSteps |> List.distinct |> List.length

    r

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())


let finished = true
()