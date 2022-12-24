module Aoc22.D24

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

type Block = WALL | EMPTY | BLIZZ of int*int

let parse2dMap (text:string) = 
    text
    |> text2lines 
    |> List.map (fun ln -> ln.ToCharArray() |> List.ofSeq |> List.map (function | '.' -> EMPTY | '#' -> WALL | '<' -> BLIZZ(-1,0) | '>' -> BLIZZ(1,0) | 'v' -> BLIZZ(0,1) | '^' -> BLIZZ(0,-1)) |> List.indexed)
    |> List.indexed
    |> List.map (fun (y, lst) -> lst |> List.map (fun (x,elt) -> (x,y),elt))
    |> List.concat

let moves = [0,0; -1,0; 1,0; 0,1; 0, -1]

let solve1 (text:string) = 
    let inp = text |> parse2dMap
    let wallSet = inp |> List.filter( fun (a,b) -> b = WALL) |> List.map fst |> Set
    let blizzards = inp |> List.filter( fun (a,b) -> match b with | BLIZZ(x,y) -> true | _ -> false)

    let pos0 = (1,0)
    let maxX = wallSet |> Seq.map fst |> Seq.max
    let maxY = wallSet |> Seq.map snd |> Seq.max
    let posN = (maxX-1, maxY)

    let dictBlizzNext = Dictionary<int, ((int*int) * Block) list>()
    let dictBlizzSetNext = Dictionary<int, (int*int) Set>()
    dictBlizzNext[0]<-blizzards
    let blizz2set b = b |> List.map fst |> Set
    dictBlizzSetNext[0] <- blizzards |> blizz2set

    
    let getNextPos (posPrev, minute) =
        let attemptPos = moves |> Seq.map ((+..) posPrev)
        let blizzNext = 
            if dictBlizzNext.ContainsKey(minute) 
            then dictBlizzNext[minute]
            else 
                let nxt: ((int*int) * Block) list = 
                    dictBlizzNext[minute-1] 
                    |> List.map (
                        fun ((bx,by),BLIZZ(dx,dy)) -> 
                            let (bx1,by1) = (bx,by) +.. (dx,dy)
                            let bx2 = match bx1 with | 0 -> maxX-1 | x when x=maxX -> 1 | _ -> bx1
                            let by2 = match by1 with | 0 -> maxY-1 | y when y=maxY -> 1 | _ -> by1
                            (bx2,by2),BLIZZ(dx,dy)
                    )
                dictBlizzNext[minute] <- nxt
                dictBlizzSetNext[minute] <- nxt |> blizz2set
                nxt

        let blizzSet = dictBlizzSetNext[minute]
        let actualPos = 
            attemptPos 
            |> Seq.filter(fun pos -> not(wallSet.Contains pos) && not(blizzSet.Contains pos))
            |> Seq.filter(fun (x,y) -> x >= 0 && x <= maxX && y >=0 && y <= maxY)
            |> List.ofSeq
        actualPos |> Seq.map(fun ap -> ap,minute+1)

    let resList =
        Common.Dijkstra (pos0, 0) getNextPos (fun (pos,_) -> pos=posN)

    let res = resList |> Seq.head |> snd

    res-1

    
let solve2 (text:string) =
    let inp = text |> parse2dMap
    let wallSet = inp |> List.filter( fun (a,b) -> b = WALL) |> List.map fst |> Set
    let blizzards = inp |> List.filter( fun (a,b) -> match b with | BLIZZ(x,y) -> true | _ -> false)

    let pos0 = (1,0)
    let maxX = wallSet |> Seq.map fst |> Seq.max
    let maxY = wallSet |> Seq.map snd |> Seq.max
    let posN = (maxX-1, maxY)

    let dictBlizzNext = Dictionary<int, ((int*int) * Block) list>()
    let dictBlizzSetNext = Dictionary<int, (int*int) Set>()
    dictBlizzNext[0]<-blizzards
    let blizz2set b = b |> List.map fst |> Set
    dictBlizzSetNext[0] <- blizzards |> blizz2set

    
    let getNextPos (posPrev, minute) =
        let attemptPos = moves |> Seq.map ((+..) posPrev)
        let blizzNext = 
            if dictBlizzNext.ContainsKey(minute) 
            then dictBlizzNext[minute]
            else 
                let nxt: ((int*int) * Block) list = 
                    dictBlizzNext[minute-1] 
                    |> List.map (
                        fun ((bx,by),BLIZZ(dx,dy)) -> 
                            let (bx1,by1) = (bx,by) +.. (dx,dy)
                            let bx2 = match bx1 with | 0 -> maxX-1 | x when x=maxX -> 1 | _ -> bx1
                            let by2 = match by1 with | 0 -> maxY-1 | y when y=maxY -> 1 | _ -> by1
                            (bx2,by2),BLIZZ(dx,dy)
                    )
                dictBlizzNext[minute] <- nxt
                dictBlizzSetNext[minute] <- nxt |> blizz2set
                nxt

        let blizzSet = dictBlizzSetNext[minute]
        let actualPos = 
            attemptPos 
            |> Seq.filter(fun pos -> not(wallSet.Contains pos) && not(blizzSet.Contains pos))
            |> Seq.filter(fun (x,y) -> x >= 0 && x <= maxX && y >=0 && y <= maxY)
            |> List.ofSeq
        actualPos |> Seq.map(fun ap -> ap,minute+1)

    let (posTrip1,minuteTrip1) = Common.Dijkstra (pos0, 0) getNextPos (fun (pos,_) -> pos=posN) |> Seq.head
    let (posTrip2,minuteTrip2) = Common.Dijkstra (posTrip1, minuteTrip1) getNextPos (fun (pos,_) -> pos=pos0) |> Seq.head
    let (posTrip3,minuteTrip3) = Common.Dijkstra (posTrip2, minuteTrip2) getNextPos (fun (pos,_) -> pos=posN) |> Seq.head

    let res = minuteTrip3 - 1
    res


//let res1 = input |> solve1
//let res2 = input |> solve2

//ClipboardService.SetText(res2.ToString())


let finished = true
()
