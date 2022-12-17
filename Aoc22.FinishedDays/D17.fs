module Aoc22.D17

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

let parseLine (ln:string) =
    ln.ToCharArray() |>Seq.toList |> List.map (function | '>' -> +1 | '<' -> -1 | ch -> failwith ("huh" + ch.ToString()))
    // |> text2tokens "x"
    // |> text2tokensStr ["abc";"def"]

let parse2lines (text:string) = 
    text
    |> text2lines 
    // |> Input.list2groups ((=)"")
    |> List.map parseLine
    |> List.head

let shapes =
    [
        [0,0;1,0;2,0;3,0]
        [1, -2; 0, -1; 1, -1; 2, -1; 1, 0]
        [2, -2; 2, -1; 0,0;1,0;2,0]
        [0, -3; 0, -2; 0, -1; 0,0]
        [0, -1; 1, -1; 0, 0; 1, 0]
    ]

let solve1 (text:string) = 
    let inp = text |> parse2lines

    let numRocks = 2022

    let set0 = [0..6] |> List.map (fun x -> x,1) |> Set.ofList

    let spawnRock st shapeIdx =
        let miny = st |> Set.toSeq |> Seq.map snd |> Seq.min
        let spawny = miny-4
        let spawnx = 2

        let shape = shapes[shapeIdx%shapes.Length]
        let coords = shape |> List.map (fun p -> p +.. (spawnx,spawny))
        coords

    let moveRock (st: Set<int*int>) rock magmaIdx =
        let finalRock, finalMagmaIdx, _ =
            (rock, magmaIdx, false)
            |> Seq.unfold (
                fun (r,mi,isFall) ->
                    let dx = if isFall then 0 else inp[mi%inp.Length]
                    let dy = if isFall then +1 else 0
                    let rNextAttempt = r |> List.map ((+..) (dx,dy))
                    let isCollided = rNextAttempt |> Seq.filter (fun (px,py)-> st.Contains (px,py) || px < 0 || px > 6) |> Seq.length |> (fun l -> l>0)
                    if isCollided && isFall
                    then None 
                    else
                        let rNext = if isCollided then r else rNextAttempt
                        let next = rNext,(if dx=0 then mi else mi+1), not isFall
                        Some(next,next)
            )
            |> Seq.last
        let nextSet = finalRock |> List.fold (fun sti pos -> sti |> Set.add pos) st
        nextSet,finalMagmaIdx

    let st0 = 0,set0,0

    let finalState =
        [1..numRocks]
        |> List.fold (
            fun (shapeIdx,rockSet,magmaIdx) _ ->
                let rock0 = spawnRock rockSet shapeIdx
                let nextRockSet,nextMagmaIdx = moveRock rockSet rock0 magmaIdx
                shapeIdx+1,nextRockSet,nextMagmaIdx
        ) st0

    let (_,finalSet,_) = finalState

    let r = finalSet |> Set.toSeq |> Seq.map snd |> Seq.min |> abs |> (+)1

    //// the silliest print function. it worked...
    //let printed = finalSet |>Set.toSeq |>Seq.filter (fun (x,y) -> y <= 0) |> List.ofSeq |> List.sortBy (fun pos -> snd pos,fst pos)
    //let mutable prevY = 1
    //let mutable prevX = 1
    //for (x,y) in printed do
    //    if prevY <> y then Console.WriteLine(); prevX <- 0; else ()
    //    for ix in [prevX+2 .. x] do Console.Write(' ')
    //    Console.Write("#")
    //    prevY <- y
    //    prevX <- x

    // not 4226
    r
    
let solve2 (text:string) =
    let inp = text |> parse2lines
    
    // 208->1933 = 2709
    let numRocksOrig = 1000000000000L

    //let numRocks = 2022L

    let set0 = [0L..6L] |> List.map (fun x -> x,0L) |> Set.ofList

    let pruneSet (st: Set<int64*int64>) =
        // try it the simple way - just prune an arbitrary number of lines until it doesn't hang
        st
        |> Set.toSeq
        |> Seq.filter (fun (x,y) -> y <= 30L)
        |> Set
        
    let st0 = 0,set0,0,0L
    let dict = [(0,set0,0),0L] |> Map.ofList |> Dictionary // mutable
    dict.Clear()
    let dictDepth = [-1L,0L] |> Map.ofList |> Dictionary

    let moveSetDownwards diffy (st: Set<int64*int64>) =
        st
        |> Set.toSeq
        |> Seq.map (fun (x,y) -> x,y-diffy)
        |> set

    let spawnRock st shapeIdx =
        let spawny = -4L //miny-4L
        let spawnx = 2L

        let shape = shapes[shapeIdx%shapes.Length]
        let coords = shape |> List.map (fun (px,py) -> (int64 px + spawnx, int64 py + spawny))
        coords


    let moveRock (st: Set<int64*int64>) (rock:(int64*int64) list) magmaIdx =
        let finalRock, finalMagmaIdx, _ =
            (rock, magmaIdx, false)
            |> Seq.unfold (
                fun (r,mi,isFall) ->
                    let dx = (if isFall then 0 else inp[mi%inp.Length]) |> int64
                    let dy = (if isFall then +1 else 0) |> int64
                    let rNextAttempt = r |> List.map (fun (px,py) -> (dx+px,dy+py))
                    let isCollided = rNextAttempt |> Seq.filter (fun (px,py)-> st.Contains (px,py) || px < 0 || px > 6) |> Seq.length |> (fun l -> l>0)
                    if isCollided && isFall
                    then None 
                    else
                        let rNext = if isCollided then r else rNextAttempt
                        let next = rNext,(if dx=0 then mi else mi+1), not isFall
                        Some(next,next)
            )
            |> Seq.last
        let nextSet = finalRock |> List.fold (fun sti pos -> sti |> Set.add pos) st
        nextSet,finalMagmaIdx

    let (repetitionDepth, repetitionSize) = 
        seq { 0L..numRocksOrig-1L }
        |> Seq.scan (
            fun ((shapeIdx,(rockSet:Set<int64*int64>),magmaIdx,depth),repetitionInfo) rocki ->
                let diffy = rockSet |> Set.toSeq |> Seq.map snd |> Seq.min
                let rockSetNext = rockSet |> moveSetDownwards diffy |> pruneSet
                
                let key = shapeIdx,rockSetNext,magmaIdx
                if dict.ContainsKey(key)
                then
                    let prevIdx = dict[key]
                    let nextIdx = rocki
                    let diffy = depth - dictDepth[prevIdx]
                    
                    (shapeIdx, rockSet,magmaIdx,depth), Some (abs(diffy), nextIdx-prevIdx)


                else 
                    dict[key]<-rocki
                    dictDepth[rocki] <- depth

                    let rock0 = spawnRock rockSetNext shapeIdx

                    let nextRockSet,nextMagmaIdx = moveRock rockSetNext rock0 magmaIdx


                    ((shapeIdx+1)%shapes.Length,nextRockSet,(nextMagmaIdx%inp.Length),depth+diffy), None
        ) (st0,None)
        |> Seq.pick snd

    //let repetitionDepth = 2709L
    //let repetitionSize = 1725L
    let skippedRepetitions = (numRocksOrig / repetitionSize) - 1L
    let numRocks = numRocksOrig - skippedRepetitions * repetitionSize
    let bonusDepth = skippedRepetitions * repetitionDepth

    let finalState =
        seq { 0L..numRocks-1L }
        |> Seq.fold (
            fun (shapeIdx,(rockSet:Set<int64*int64>),magmaIdx,depth) rocki ->
                let diffy = rockSet |> Set.toSeq |> Seq.map snd |> Seq.min
                let rockSetNext = rockSet |> moveSetDownwards diffy |> pruneSet
                
                let rock0 = spawnRock rockSetNext shapeIdx

                let nextRockSet,nextMagmaIdx = moveRock rockSetNext rock0 magmaIdx

                (shapeIdx+1)%shapes.Length,nextRockSet,(nextMagmaIdx%inp.Length),depth+diffy
        ) st0

    let (_,finalSet,_,finalDepth) = finalState

    let rManual = finalSet |> Set.toSeq |> Seq.map snd |> Seq.min |> abs |> (+)(abs finalDepth)


    let rFinal = rManual + bonusDepth
    rFinal

//let res1 = input |> solve1
//let res2 = input |> solve2

//ClipboardService.SetText(res2.ToString())


let finished = true
()