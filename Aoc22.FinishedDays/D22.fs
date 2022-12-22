module Aoc22.D22

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
    ln.ToCharArray()
    |> List.ofSeq
    |> List.map (function | '#' -> Some(true) | '.' -> Some(false) | _ -> None)
    |> List.indexed
    |> List.filter (fun (i,x) -> x<>None)
    |> List.map (fun (i,x) -> i,x.Value)
    // |> text2tokens "x"
    // |> text2tokensStr ["abc";"def"]

    
let parseIns (ln:string) =
    ln.ToCharArray() 
    |> List.ofSeq 
    |> List.fold (
        fun st ch -> 
            if Char.IsDigit(ch)
            then
                let num = ch.ToString() |> int
                match st with
                | (0, ch)::tail -> (num, ch)::tail
                | (n, '_')::tail -> (n*10+num, '_')::tail
                | _ -> (num,'_')::st
            else
                let (head,defCh)::tail=st
                (head,ch)::tail
    ) ([])
    // |> text2tokens "x"
    // |> text2tokensStr ["abc";"def"]



let parse2lines (text:string) = 
    text
    |> text2lines 
    |> Input.list2groups ((=)"")
    |> fun grps ->
        let mpList = 
            grps[0] 
            |> List.map parseLine
            |> List.mapi (fun y ln -> ln |> List.map (fun (x,ch) -> (x,y),ch))
            |> List.concat
        let ins = grps[1][0] |> parseIns |> List.rev

        mpList,ins


let left (dx,dy) =
    match (dx,dy) with
    | 1,0 -> 0, -1
    | 0, -1 -> -1,0
    | -1, 0 -> 0, 1
    | 0, 1 -> 1,0

let right (dx,dy) =
    match (dx,dy) with
    | 1,0 -> 0, 1
    | 0, 1 -> -1,0
    | -1, 0 -> 0, -1
    | 0, -1 -> 1,0


let solve1 (text:string) = 
    let (mpLst,listIns) = text |> parse2lines
    let mp = mpLst |> Map.ofList

    let getMinX argy =
        let firstX = mpLst |> List.map fst |> List.filter (fun (x,y) -> y=argy) |> List.map fst |> List.min
        firstX,argy
    let getMaxX argy =
        let lastX = mpLst |> List.map fst |> List.filter (fun (x,y) -> y=argy) |> List.map fst |> List.max
        lastX,argy
    let getMinY argx =
        let firstY = mpLst |> List.map fst |> List.filter (fun (x,y) -> x=argx) |> List.map snd |> List.min
        argx,firstY
    let getMaxY argx =
        let lastY = mpLst |> List.map fst |> List.filter (fun (x,y) -> x=argx) |> List.map snd |> List.max
        argx,lastY

    let wrapAround (x,y) (dx,dy) =
        let w = match (dx,dy) with | -1,0 -> getMaxX y | 1,0 -> getMinX y | 0, -1 -> getMaxY x | 0, 1 -> getMinY x
        w


    let firstX = mpLst |> List.map fst |> List.filter (fun (x,y) -> y=0) |> List.map fst |> List.min
    let pos0 = (firstX,0)
    let st0 = pos0,(1,0)

    let fw1 (pos,dir) =
        let attempt = pos +.. dir
        
        if mp.ContainsKey(attempt) 
        then 
            if mp[attempt] 
            then pos 
            else attempt
        else
            let wrappedAttempt = wrapAround attempt dir
            if mp[wrappedAttempt] then pos else wrappedAttempt

    let fwn n (pos : int*int) (dir : int*int) =
        [1..n]
        |> List.fold (fun ((p:int*int),(d:int*int)) _ -> fw1 (p,d), d) (pos,dir)

    let (xn,yn),(dxn,dyn) =
        listIns
        |> List.fold (
            fun (posPrev,dirPrev) (num,ch) -> 
                let posNext = fwn num posPrev dirPrev |> fst
                let dirNext = match ch with | 'L' -> left dirPrev | 'R' -> right dirPrev | '_' -> dirPrev
                posNext,dirNext
        ) st0

    let puzzleColumn = xn+1
    let puzzleRow = yn+1
    let puzzleDir = match (dxn,dyn) with | 1,0 -> 0 | 0,1 -> 1 | -1, 0 -> 2 | 0, -1 -> 3

    let r = 1000 * puzzleRow + 4 * puzzleColumn + puzzleDir

    r
    
let solve2 (text:string) =
    let (mpLst,listIns) = text |> parse2lines
    let mp = mpLst |> Map.ofList

    let wrapAround (x,y) (dx,dy) =
        // calculate face
        let facex = x / 50
        let facey = y / 50

        // calculate position within face
        let relX = x%50
        let relY = y%50

        // manually considered each face and where it would go for my specific input, which looked like this:
        // |  12 |
        // |  3  |
        // | 45  |
        // | 6   |
        // For example, face 1 has facex,facey = 1,0

        let (x2,y2),(dx2,dy2) =
            match (facex,facey),(dx,dy) with

            // face 3 <-> face 4
            | (1,1),(-1,0) -> 
                (relY,50*2),(0,1)
            | (0,2),(0, -1) ->
                (50*1,50+relX),(1,0)

            // face 1 <-> face 4
            | (1,0),(-1,0) -> 
                (0,50*3-1-relY),(1,0)
            | (0,2),(-1,0) ->
                (50,50-1-relY),(1,0)

            // face 1 <-> face 6
            | (1,0),(0, -1) ->
                (0,3*50+relX),(1,0)
            | (0,3),(-1, 0) ->
                (50+relY,0),(0,1)

            // face 2 <-> face 6
            | (2,0),(0, -1) ->
                (relX,4*50-1),(0,-1)
            | (0,3),(0,1) ->
                (2*50+relX,0),(0,1)

            // face 2 <-> face 5
            | (2,0),(1,0) ->
                (2*50-1,3*50-1-relY),(-1,0)
            | (1,2),(1,0) ->
                (3*50-1,50-1-relY),(-1,0)

            // face 2 <-> face 3
            | (2,0),(0,1) ->
                (2*50-1,50+relX),(-1,0)
            | (1,1),(1,0) ->
                (2*50+relY,50-1),(0, -1)

            // face 5 <-> face 6
            | (1,2),(0,1) ->
                (50-1,3*50+relX),(-1,0)
            | (0,3),(1,0) ->
                (50+relY,3*50-1),(0, -1)
        (x2,y2),(dx2,dy2)


    let firstX = mpLst |> List.map fst |> List.filter (fun (x,y) -> y=0) |> List.map fst |> List.min
    let pos0 = (firstX,0)
    let st0 = pos0,(1,0)

    let fw1 (pos,dir) =
        let attempt = pos +.. dir
        
        if mp.ContainsKey(attempt) 
        then 
            if mp[attempt] 
            then pos,dir
            else attempt,dir
        else
            let wrappedAttemptPos,wrappedAttemptDir = wrapAround pos dir
            if mp[wrappedAttemptPos] then pos,dir else wrappedAttemptPos,wrappedAttemptDir

    let fwn n (pos : int*int) (dir : int*int) =
        [1..n]
        |> List.fold (fun ((p:int*int),(d:int*int)) _ -> fw1 (p,d)) (pos,dir)

    let (xn,yn),(dxn,dyn) =
        listIns
        |> List.fold (
            fun (posPrev,dirPrev) (num,ch) -> 
                let posNext,dirNext1 = fwn num posPrev dirPrev
                let dirNext2 = match ch with | 'L' -> left dirNext1 | 'R' -> right dirNext1 | '_' -> dirNext1
                posNext,dirNext2
        ) st0

    let puzzleColumn = xn+1
    let puzzleRow = yn+1
    let puzzleDir = match (dxn,dyn) with | 1,0 -> 0 | 0,1 -> 1 | -1, 0 -> 2 | 0, -1 -> 3

    let r = 1000 * puzzleRow + 4 * puzzleColumn + puzzleDir

    r

//let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())


let finished = true
()