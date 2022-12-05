module Aoc22.D05

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

let parseLineGrp1 (ln:string) =
    ln.ToCharArray()
    |> List.ofArray
    |> List.chunkBySize 4
    |> map (fun chnk -> if chnk[1]=' ' then None else Some chnk[1])
    // |> text2tokensStr ["abc";"def"]

let parseLineGrp2 (ln:string) =
    ln
    |> text2tokensStr [" ";"move";"from";"to"]
    |> fun xs -> int xs[0],int xs[1], int xs[2]

let parse2lines (text:string) = 
    text
    |> text2lines 
    |> Input.list2groups ((=)"")
    |> fun (grp1::[grp2]) ->
        let lns1 = grp1 |> List.rev |> List.skip 1 |> map parseLineGrp1 |> List.rev |> List.transpose |> map (List.choose id) // |> map (fun chars -> System.String(chars |>Array.ofSeq))
        let lns2 = grp2 |> map parseLineGrp2
        lns1,lns2

let solve1 (text:string) = 
    let stack0, listINS = text |> parse2lines

    let stackN = 
        listINS
        |> List.fold (
            fun stack (howMany, from, xTo) ->
                let realFrom=from-1
                let realTo = xTo-1
                let stackMap = stack |> List.indexed |> Map.ofList 
                let stackNext = 
                    //// original code of this block commented here
                    //seq {
                    //    for i in [0.. (stack |> List.length)-1] do
                    //        if i = from-1 then
                    //            stack[i] |> List.skip howMany
                    //        elif i = xTo - 1 then
                    //            stack[i] |> List.append (stack[from-1] |> List.take howMany |> List.rev)
                    //        else stack[i]
                    //} |>List.ofSeq
                    stackMap
                    |> Map.add realFrom (stackMap[realFrom] |> List.skip howMany)
                    |> Map.add realTo (List.append (stackMap[realFrom] |> List.take howMany |> List.rev) stackMap[realTo])
                    |> Map.values
                    |> List.ofSeq
                stackNext
        ) stack0

    let r = stackN |> map List.head |> fun chars -> System.String(chars |>Array.ofSeq)

    r

let solve2 (text:string) = 
    let stack0, listINS = text |> parse2lines

    let stackN = 
        listINS
        |> List.fold (
            fun stack (howMany, from, xTo) ->
                let realFrom=from-1
                let realTo = xTo-1
                let stackMap = stack |> List.indexed |> Map.ofList 
                let stackNext = 
                    stackMap
                    |> Map.add realFrom (stackMap[realFrom] |> List.skip howMany)
                    |> Map.add realTo (List.append (stackMap[realFrom] |> List.take howMany) stackMap[realTo])
                    |> Map.values
                    |> List.ofSeq
                stackNext
        ) stack0

    let r = stackN |> map List.head |> fun chars -> System.String(chars |>Array.ofSeq)

    r
    
let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res1.ToString())


let finished = true
()