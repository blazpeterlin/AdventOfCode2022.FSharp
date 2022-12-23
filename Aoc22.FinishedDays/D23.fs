module Aoc22.D23

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
    ln.ToCharArray() |> List.ofSeq |> List.map (function | '.' -> false | '#' ->true) |> List.indexed
    // |> text2tokens "x"
    // |> text2tokensStr ["abc";"def"]

let parse2lines (text:string) = 
    text
    |> text2lines 
    // |> Input.list2groups ((=)"")
    |> List.map parseLine
    |> List.indexed
    |> List.map (fun (y, lst) -> lst |> List.map (fun (x,elt) -> (x,y),elt))
    |> List.concat

let moves8 = [1,0;1,1;0,1; -1,1; -1,0; -1, -1; 0, -1; 1, -1]

let movesN = [-1, -1;0, -1; 1, -1],(0,-1)
let movesS = [-1, 1;0, 1; 1, 1],(0,1)
let movesW = [-1, -1; -1, 0; -1, 1],(-1,0)
let movesE = [1, -1; 1, 0; 1, 1],(1,0)

let initialMoves = [movesN;movesS;movesW;movesE]



let solve1 (text:string) = 
    let inp0 = text |> parse2lines
    let set0  = inp0 |> List.filter(fun ((x,y),isElf) -> isElf) |> List.map fst |> Set

    
    //Common.print2d (set0)
    //Console.WriteLine ""
    //Console.WriteLine ""

    let step (seti: Set<int*int>) (lstMoves: ((int*int) list*(int*int)) list) =
        let locs = seti |> List.ofSeq
        let proposedLocs =
            locs
            |> Seq.map (
                fun (x,y) ->
                    let nbrs = moves8 |> List.map ((+..) (x,y)) |>List.filter (seti.Contains)
                    if nbrs.Length=0 then (x,y) else
                    List.append lstMoves ([List.empty,(0,0)])
                    |> Seq.choose (
                        fun (checks,move) ->
                            let nbrsC = checks |> Seq.map ((+..) (x,y)) |> Seq.filter (seti.Contains) |> List.ofSeq
                            if nbrsC.Length=0 then Some((x,y)+..move) else None
                    ) |> Seq.head
            )
            |> List.ofSeq
        let doubledLocs = proposedLocs |> List.countBy id |> List.filter (fun (_,cnt) -> cnt>1) |> List.map fst |> Set
        let finalLocs =
            List.zip locs proposedLocs
            |> List.map (fun (loc,proposedLoc) ->
                if doubledLocs.Contains proposedLoc then loc else proposedLoc
            )
            |> List.sortBy (fun p -> snd p , fst p)

        let finalSet = finalLocs |> Set
        let hd = List.head lstMoves
        let finalLstMoves = (List.tail lstMoves)@[hd]

        Common.print2dBounds finalLocs 0 0 5 5

        Console.WriteLine ""
        Console.WriteLine "----------"
        Console.WriteLine ""

        finalSet, finalLstMoves

    let finalSet = 
       [1..10]
        |> List.fold (
            fun (seti, lstMoves) i -> step seti lstMoves
        ) (set0, initialMoves)
        |> fst

    let finalSeq = finalSet |> List.ofSeq
        
    let minX = finalSeq |> Seq.map (fun ((x,y)) -> x) |> Seq.min
    let minY = finalSeq |> Seq.map (fun ((x,y)) -> y) |> Seq.min
    let maxX = finalSeq |> Seq.map (fun ((x,y)) -> x) |> Seq.max
    let maxY = finalSeq |> Seq.map (fun ((x,y)) -> y) |> Seq.max

    let r = (maxY - minY + 1) * (maxX - minX + 1) - (Seq.length finalSeq)

    r
    
let solve2 (text:string) =
    let inp0 = text |> parse2lines
    let set0  = inp0 |> List.filter(fun ((x,y),isElf) -> isElf) |> List.map fst |> Set


    let step (seti: Set<int*int>) (lstMoves: ((int*int) list*(int*int)) list) =
        let locs = seti |> List.ofSeq
        let proposedLocs =
            locs
            |> Seq.map (
                fun (x,y) ->
                    let nbrs = moves8 |> List.map ((+..) (x,y)) |>List.filter (seti.Contains)
                    if nbrs.Length=0 then (x,y) else
                    List.append lstMoves ([List.empty,(0,0)])
                    |> Seq.choose (
                        fun (checks,move) ->
                            let nbrsC = checks |> Seq.map ((+..) (x,y)) |> Seq.filter (seti.Contains) |> List.ofSeq
                            if nbrsC.Length=0 then Some((x,y)+..move) else None
                    ) |> Seq.head
            )
            |> List.ofSeq
        let doubledLocs = proposedLocs |> List.countBy id |> List.filter (fun (_,cnt) -> cnt>1) |> List.map fst |> Set
        let finalLocs =
            List.zip locs proposedLocs
            |> List.map (fun (loc,proposedLoc) ->
                if doubledLocs.Contains proposedLoc then loc else proposedLoc
            )
            |> List.sortBy (fun p -> snd p , fst p)

        let finalSet = finalLocs |> Set
        let hd = List.head lstMoves
        let finalLstMoves = (List.tail lstMoves)@[hd]

        finalSet, finalLstMoves

    let (finalIdx,_), _ = 
        Seq.initInfinite id
        |> Seq.scan (
            fun (seti, lstMoves) i -> step seti lstMoves
        ) (set0, initialMoves)
        |> Seq.indexed
        |> Seq.pairwise
        |> Seq.find (fun ((idx1,(st1,_)),(idx2,(st2,_))) -> st1=st2)

    let r = finalIdx+1

    r

//let res1 = input |> solve1
//let res2 = input |> solve2

//ClipboardService.SetText(res2.ToString())


//let finished = true
()