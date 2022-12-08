module Aoc22.D08

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
    ln.ToCharArray() |> List.ofArray |> map (fun ch -> ch.ToString() |> int)
    // |> text2tokens "x"
    // |> text2tokensStr ["abc";"def"]

let parse2lines (text:string) = 
    text
    |> text2lines 
    // |> Input.list2groups ((=)"")
    |> List.map parseLine
    |> List.mapi (fun y ln -> ln |> List.mapi (fun x ch -> (x,y),ch))
    |> List.concat



let solve1 (text:string) = 
    let inp = text |> parse2lines
    let maxY = inp |> map fst |> map snd |>List.max
    let maxX = inp |> map fst |> map fst |>List.max
    let map = inp |> Map.ofList

    let leftTreesList = 
        seq {
           for y in 0..maxY do
            [0..maxX] 
            |> List.fold (fun (maxPrev,lst) x -> 
                    let nextElt = map[x,y]
                    if maxPrev < nextElt 
                    then nextElt,(x,y)::lst
                    else maxPrev,lst
                ) ( -1,[])
        }
        |> List.ofSeq
        |> List.map (fun (num,pos) -> pos)
        |> List.concat

    let rightTreesList = 
        seq {
           for y in 0..maxY do
            [maxX.. -1..0] 
            |> List.fold (fun (maxPrev,lst) x -> 
                    let nextElt = map[x,y]
                    if maxPrev < nextElt 
                    then nextElt,(x,y)::lst
                    else maxPrev,lst
                ) ( -1,[])
        }
        |> List.ofSeq
        |> List.map (fun (num,pos) -> pos)
        |> List.concat

    
    let topTreesList = 
        seq {
           for x in 0..maxX do
            [0..maxY] 
            |> List.fold (fun (maxPrev,lst) y -> 
                    let nextElt = map[x,y]
                    if maxPrev < nextElt 
                    then nextElt,(x,y)::lst
                    else maxPrev,lst
                ) ( -1,[])
        }
        |> List.ofSeq
        |> List.map (fun (num,pos) -> pos)
        |> List.concat

    let bottomTreesList = 
        seq {
           for x in 0..maxX do
            [maxY.. -1 ..0] 
            |> List.fold (fun (maxPrev,lst) y -> 
                    let nextElt = map[x,y]
                    if maxPrev < nextElt 
                    then nextElt,(x,y)::lst
                    else maxPrev,lst
                ) ( -1,[])
        }
        |> List.ofSeq
        |> List.map (fun (num,pos) -> pos)
        |> List.concat

    let r = [topTreesList ; bottomTreesList ; leftTreesList ; rightTreesList] |> List.concat |> List.distinct |> List.length

    r

let countNumTrees (mp: Map<(int*int),int>) (x0,y0) (ix,iy) (edgeX,edgeY) =
    let coords : (int*int) seq = 
        (x0,y0)
        |> Seq.unfold (fun ((x,y)) -> 
                if (x,y) = (edgeX,edgeY) 
                then None 
                else 
                    let x2:int = x+ix
                    let y2:int = y+iy
                    let r2 = (x2,y2)
                    Some (r2,r2)
            )
        //|> List.ofSeq 
    let blockedValue = mp[x0,y0]
    let forLine = 
        coords
        |> Seq.scan (fun (isBlocked,lst) (x,y) -> 
            let nextElt = mp[x,y]
            let nextLst = if (isBlocked=0) (*&& nextElt <= blockedValue*) then (x,y)::lst else lst
            let nextIsBlocked = if isBlocked=1 then 2 elif nextElt >= blockedValue then 1 else 0
            nextIsBlocked,nextLst
        ) (0,[])
        |> Seq.takeWhile (fun (isBlocked, _) -> isBlocked<=1)
        |> Seq.last
    let numTrees = forLine |> snd |> List.length
    numTrees

let scenicScore (mp: Map<(int*int),int>) maxX maxY (x0,y0) =

    //let test = if x0 =2 && y0 = 3 then 1 else 0

    let left = countNumTrees mp (x0,y0) (-1,0) (0,y0)
    let right = countNumTrees mp (x0,y0) (1,0) (maxX,y0)
    let top = countNumTrees mp (x0,y0) (0, -1) (x0,0)
    let bottom = countNumTrees mp (x0,y0) (0, +1) (x0,maxY)

    left*right*top*bottom
    
let solve2 (text:string) =
    let inp = text |> parse2lines
    let mp = inp |> Map.ofList
    let allCoords = mp |> Map.keys |> List.ofSeq
    let maxX = allCoords |> List.map fst |>List.max
    let maxY = allCoords |> List.map snd |>List.max

    let r =
        allCoords
        |> map (scenicScore mp maxX maxY)
        |> List.max

    r


let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())


let finished = true
()