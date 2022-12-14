module Aoc22.D14b

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
    |> text2tokensStr [" -> "]
    |> fun xs -> 
        xs
        |> List.map (
            fun pos ->
                pos |> text2tokens "," |> fun ys -> int ys[0], int ys[1]
            )

let parse2lines (text:string) = 
    text
    |> text2lines 
    // |> Input.list2groups ((=)"")
    |> List.map parseLine

let genWalls (pos: (int*int) list) =
    pos
    |> List.pairwise
    |> List.map (
        fun ((x0,y0),(xn,yn)) ->
            let diff = (if xn > x0 then 1 elif xn < x0 then -1 else 0),(if yn > y0 then 1 elif yn < y0 then -1 else 0)
            (x0,y0)
            |> Seq.unfold (fun (x,y) -> if (x,y)=(xn,yn) then None else 
                                        let posnext=(x,y)+..diff
                                        Some(posnext,posnext)
            )
            |> List.ofSeq
            |> fun lst -> (x0,y0)::lst
    )
    |> List.concat
    |> List.distinct


type SandStatus = Standing | Falling | Abyss


let rec produceSand maxy (wallSet : HashSet<int*int>) (sandSet : HashSet<int*int>) (x,y) =
    if wallSet.Contains(x,y) || sandSet.Contains(x,y) then None
    elif y > maxy then None
    elif 
        (      (wallSet.Contains (x,y+1) || sandSet.Contains(x,y+1))
            && (wallSet.Contains(x-1,y+1) || sandSet.Contains(x-1,y+1))
            && (wallSet.Contains(x+1,y+1) || sandSet.Contains(x+1,y+1))
        )
    then Some(x,y)
    else 
        seq { (x,y+1) ; (x-1,y+1) ; (x+1,y+1) } 
        |> Seq.find (fun pos -> not (wallSet.Contains pos || sandSet.Contains(pos)))
        |> produceSand maxy wallSet sandSet

let solve1 (text:string) = 
    let inp = text |> parse2lines
    let walls = inp |> List.map genWalls |> List.concat |> List.distinct
    let maxy = walls |>List.map snd |> List.max

    let wallSet = walls |> HashSet // mutable

    let pos0 = 500,0
    let s0 = []
    let sandSet = s0 |> HashSet // mutable


    let numSand = 
        Seq.initInfinite id
        |> Seq.takeWhile (
            fun _ ->
                let nextS = produceSand maxy wallSet sandSet pos0
                match nextS with | None ->false | Some s -> sandSet.Add(s)
        )
        |> Seq.last
        |> (+)1

    numSand
    
let solve2 (text:string) =
    let inp = text |> parse2lines
    let walls = inp |> List.map genWalls |> List.concat |> List.distinct
    let maxy = walls |>List.map snd |> List.max
    let extraWalls = [-1000 .. 1000] |> List.map (fun x -> (x,maxy+2))
    let wallSet = walls@extraWalls |> HashSet // mutable

    let pos0 = 500,0
    let s0 = []
    let sandSet = s0 |> HashSet // mutable


    let numSand = 
        Seq.initInfinite id
        |> Seq.takeWhile (
            fun _ ->
                if sandSet.Contains pos0 then false else
                let nextS = produceSand (maxy+999) wallSet sandSet pos0
                match nextS with | None ->false | Some s -> sandSet.Add(s)
                
        )
        |> Seq.last
        |> (+)1

    numSand

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res1.ToString())


let finished = true
()