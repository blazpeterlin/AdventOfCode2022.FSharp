module Aoc22.D20

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
    ln |> int

let parse2lines (text:string) = 
    text
    |> text2lines
    |> List.map parseLine
    
let getPrev (node:LinkedListNode<'a>) =
    if node = node.List.First then node.List.Last else node.Previous
    
let getPrevN num (node:LinkedListNode<'a>) =
    [1 .. num]
    |> List.fold (fun n _ ->
        getPrev n
    ) node

let getNext (node:LinkedListNode<'a>) =
    if node = node.List.Last then node.List.First else node.Next

let getNextN num (node:LinkedListNode<'a>) =
    [1 .. num]
    |> List.fold (fun n _ ->
        getNext n
    ) node

let findNode (ll: LinkedList<'a>) idx =
    let n0 = ll.First
    getNextN idx n0

let solve1 (text:string) = 
    let inp = text |> parse2lines
    let len = inp.Length

    let zeroIdx = inp |> List.findIndex ((=)0)

    let ll = LinkedList<int*int>()
    for i in List.indexed inp do
        ll.AddLast(i)

    for idx in 0 .. inp.Length - 1  do
        let node = ll.Find (idx,inp[idx])
        let nodeVal = node.Value
        //let nodeValPos = if nodeVal > 0 then (nodeVal % inp.Length) else -((-nodeVal) % inp.Length) //((nodeVal + (if nodeVal >0 then 0 else -1) + 1000 * inp.Length) % inp.Length)
        let _,nodeValPos = nodeVal
        
        let nodeFinal = 
            if nodeValPos > 0
            then
                let nodeNext = getNext node
                ll.Remove(node)
                getNextN nodeValPos nodeNext
            else
                let nodePrev = getPrev node
                ll.Remove(node)
                getPrevN (abs nodeValPos) nodePrev

        if nodeValPos > 0
        then ll.AddBefore(nodeFinal, nodeVal) |> ignore
        else ll.AddAfter(nodeFinal, nodeVal) |> ignore

        0 |> ignore

        //if (getNext n) = addedNode then addedNode else n

    let index0 = ll.Find(zeroIdx,0)
    let indexesAfter0 = [1000;2000;3000]

    let valsAfter0 = indexesAfter0 |>List.map(fun i -> getNextN i index0) |> List.map(fun node -> node.Value)
    let r = valsAfter0 |> List.map snd |> List.sum

    r


let parseLineL (ln:string) =
    ln |> int64

let parse2linesL (text:string) = 
    text
    |> text2lines
    |> List.map parseLineL
    
let solve2 (text:string) =
    let decKey = 811589153L
    let inp = text |> parse2linesL |>List.map ((*)decKey)
    let len = inp.Length |> int64

    let zeroIdx = inp |> List.findIndex ((=)0L)

    let ll = LinkedList<int*int64>()
    for i in List.indexed inp do
        ll.AddLast(i)

    for mixings in 1..10 do
        for idx in 0 .. inp.Length - 1  do
            let node = ll.Find (idx,inp[idx])
            let nodeVal = node.Value
            //let nodeValPos = if nodeVal > 0 then (nodeVal % inp.Length) else -((-nodeVal) % inp.Length) //((nodeVal + (if nodeVal >0 then 0 else -1) + 1000 * inp.Length) % inp.Length)
            let _,nodeValPos0 = nodeVal
            let nodeValPos = if nodeValPos0 > 0 then (nodeValPos0 % (len-1L)) else -((-nodeValPos0) % (len-1L)) 
        
            let nodeFinal = 
                if nodeValPos > 0
                then
                    let nodeNext = getNext node
                    ll.Remove(node)
                    getNextN (int nodeValPos) nodeNext
                else
                    let nodePrev = getPrev node
                    ll.Remove(node)
                    getPrevN (abs (int nodeValPos)) nodePrev

            if nodeValPos > 0
            then ll.AddBefore(nodeFinal, nodeVal) |> ignore
            else ll.AddAfter(nodeFinal, nodeVal) |> ignore

            0 |> ignore

    let index0 = ll.Find(zeroIdx,0)
    let indexesAfter0 = [1000;2000;3000]

    let valsAfter0 = indexesAfter0 |>List.map(fun i -> getNextN i index0) |> List.map(fun node -> node.Value)
    let r = valsAfter0 |> List.map snd |> List.sum

    r

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res1.ToString())


let finished = true
()