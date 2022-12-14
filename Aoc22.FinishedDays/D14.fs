module Aoc22.D14

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

let solve1 (text:string) = 
    let inp = text |> parse2lines
    let walls = inp |> List.map genWalls |> List.concat |> List.distinct
    let maxy = walls |>List.map snd |> List.max

    let wallSet = walls |> HashSet // mutable

    let pos0 = 500,0
    let s0 = []
    let sandSet = s0 |> HashSet // mutable

    let getPotentials (x,y) =
        seq { (x,y+1) ; (x-1,y+1) ; (x+1,y+1) }

    let sandStatus (x,y) =
        if (x,y)=(494,8) then 0 else 0
        if 
            (      (wallSet.Contains (x,y+1) || sandSet.Contains(x,y+1))
                && (wallSet.Contains(x-1,y+1) || sandSet.Contains(x-1,y+1))
                && (wallSet.Contains(x+1,y+1) || sandSet.Contains(x+1,y+1))
            )
            then Standing
        elif y >= maxy then Abyss
        else Falling


    let rec moveSand (x0,y0) : (int*int) option =
        if (x0,y0)=(494,8) then 0 else 0

        if wallSet.Contains (x0,y0) || sandSet.Contains(x0,y0) then None else
        match sandStatus (x0,y0) with
        | Abyss -> Some (0, maxy+1)
        | Standing -> Some (x0,y0)
        | _ ->

            (x0,y0)
            |> Seq.unfold (fun (x,y) -> 
                            match sandStatus (x,y) with
                            | Abyss -> None
                            | Standing -> None
                            | Falling -> 
                                match getPotentials (x,y) |> Seq.tryPick moveSand with
                                | None -> 
                                    let r = Abyss,(x,y)
                                    Some(r,(x,y))
                                | Some(xn,yn) -> 
                                    let r = Standing,(xn,yn)
                                    Some(r,(xn,yn))
            )
            |> Seq.filter (fun (status,pn) -> status=Standing)
            |> Seq.map snd
            |> Seq.tryHead
       
    let huh = moveSand (500,1)
    
    let singleSandProduce (x0,y0) =
        let nextSand = 
            getPotentials (x0,y0)
            |> Seq.tryPick (
                fun (x,y) -> moveSand (x,y) 
            )

        match nextSand with
        | Some(x,y) when y > maxy -> false
        | Some (x,y) -> sandSet.Add (x,y)
        | None ->false

        nextSand

    let r = 
        sandSet.Count
        |> Seq.unfold (
            fun sands ->
                let count = sandSet.Count

                let producedSands = [pos0] |> List.ofSeq |> List.choose (singleSandProduce)
                //for ps in producedSands do
                //    if wallSet.Contains(ps) then sandSet.Remove(ps) else false

                let count2 = sandSet.Count

                if count2 = count 
                then
                    None
                else Some(count2,count2)

        )
        |> Seq.last



    let printable = sandSet |> List.ofSeq |> List.sortBy (fun s -> snd s, fst s)

    r 
    
let solve2 (text:string) =
    let inp = text |> parse2lines
    let walls0 = inp |> List.map genWalls |> List.concat |> List.distinct
    let maxyForBot = walls0 |>List.map snd |> List.max
    let wallsAtBotom = [500-maxyForBot-10 .. 500+maxyForBot+10] |> List.map (fun x -> (x,maxyForBot+2))
    let walls = walls0@wallsAtBotom

    let wallSet = walls |> HashSet // mutable

    let pos0 = 500,0
    let s0 = [pos0]
    let sandSet = s0 |> HashSet // mutable

    let getPotentials (x,y) =
        seq { (x,y+1) ; (x-1,y+1) ; (x+1,y+1) }

    let sandStatus (x,y) =
        if (x,y)=(494,8) then 0 else 0
        if 
            (      (wallSet.Contains (x,y+1) || sandSet.Contains(x,y+1))
                && (wallSet.Contains(x-1,y+1) || sandSet.Contains(x-1,y+1))
                && (wallSet.Contains(x+1,y+1) || sandSet.Contains(x+1,y+1))
            )
            then Standing
        //elif y >= maxy then Abyss
        else Falling


    let rec moveSand (x0,y0) : (int*int) option =
        if (x0,y0)=(494,8) then 0 else 0

        if wallSet.Contains (x0,y0) || sandSet.Contains(x0,y0) then None else
        match sandStatus (x0,y0) with
        //| Abyss -> Some (0, maxy+1)
        | Standing -> Some (x0,y0)
        | _ ->

            (x0,y0)
            |> Seq.unfold (fun (x,y) -> 
                            match sandStatus (x,y) with
                            | Abyss -> None
                            | Standing -> None
                            | Falling -> 
                                match getPotentials (x,y) |> Seq.tryPick moveSand with
                                | None -> 
                                    let r = Abyss,(x,y)
                                    Some(r,(x,y))
                                | Some(xn,yn) -> 
                                    let r = Standing,(xn,yn)
                                    Some(r,(xn,yn))
            )
            |> Seq.filter (fun (status,pn) -> status=Standing)
            |> Seq.map snd
            |> Seq.tryHead
       
    let huh = moveSand (500,1)
    
    let singleSandProduce (x0,y0) =
        let nextSand = 
            getPotentials (x0,y0)
            |> Seq.tryPick (
                fun (x,y) -> moveSand (x,y) 
            )

        match nextSand with
        //| Some(x,y) when y > maxy -> false
        | Some (x,y) -> sandSet.Add (x,y)
        | None ->false

        nextSand

    let r = 
        sandSet.Count
        |> Seq.unfold (
            fun sands ->
                let count = sandSet.Count

                let producedSands = [pos0] |> List.ofSeq |> List.choose (singleSandProduce)

                let count2 = sandSet.Count
                if sandSet.Contains(pos0+..(-1,1)) 
                    && sandSet.Contains(pos0+..(0,1)) 
                    && sandSet.Contains(pos0+..(1,1))
                    then None
                else Some(count2,count2)

        )
        |> Seq.last
        |> fun r -> r+1


    let printable = sandSet |> List.ofSeq |> List.sortBy (fun s -> snd s, fst s)

    r 

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())


let finished = true
()