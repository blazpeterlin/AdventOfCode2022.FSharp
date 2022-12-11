module Aoc22.D11

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

type Monkey = { Items: bigint list; Op: bigint -> bigint; DivTest: bigint; ToTrue: int; ToFalse: int; Activity: int; ModuloNum: int; }

let parse2lines (text:string) = 
    text
    |> text2lines 
    |> Input.list2groups ((=)"")
    |> List.map (
        fun grp -> 
            let itemsStr = grp[1] |> text2tokens ":" |>List.last
            let items = itemsStr |> text2tokens ", " |> List.map int |> List.map bigint



            let tkns2 = grp[2] |>text2tokens "=" |> List.last |> text2tokens " "
            let opNum =  match tkns2[2] with | "old" -> 1 | x -> int x
            let getNum2 num = match tkns2[2] with | "old" -> num | x -> bigint (int x)
            let op num1 = match tkns2[1] with | "*" -> num1*(getNum2 num1) | "+" -> num1+(getNum2 num1)
            let div = grp[3] |> text2tokens " " |> List.last |> int
            let monkeyTrue = grp[4] |> text2tokens " " |> List.last |> int
            let monkeyFalse = grp[5] |> text2tokens " " |> List.last |> int
            { Items=items; Op=op; DivTest=div; ToTrue=monkeyTrue; ToFalse=monkeyFalse; Activity=0;ModuloNum=opNum*div }
        )

let solve1 (text:string) = 
    let inp = text |> parse2lines

    let st0 = inp

    let numRound = 20

    let stN : Monkey list =
        [0..numRound-1]
        |> List.fold (
            fun (sti:Monkey list) _ ->
                [0..sti.Length-1]
                |> List.fold (
                    fun st i ->
                        let m:Monkey = st[i]

                        let itemsTo =
                            m.Items
                            |> map (
                                fun item ->
                                    let item2 = m.Op item
                                    let item3 = item2/(bigint 3L)
                                    let throwTo = if item3%m.DivTest=0 then m.ToTrue else m.ToFalse
                                    item3,throwTo,i
                            )

                        let mp = st |> List.indexed |> Map.ofList
                        let mp2 : Map<int,Monkey> = 
                            itemsTo
                            |> List.fold (
                                fun imp (num,xTo,xFrom) ->
                                    let monkeyTo : Monkey = imp[xTo]
                                    let nextItems = monkeyTo.Items @ [num]
                                    let imp2 = imp |> Map.add xTo {monkeyTo with Items=nextItems}

                                    let monkeyFrom : Monkey = imp[xFrom]
                                    let imp3 = imp2 |> Map.add xFrom {monkeyFrom with Activity=monkeyFrom.Activity+1; Items=[]}
                                    imp3
                            ) mp

                        mp2.Values |> List.ofSeq
                ) sti
                    





        ) st0

    
    let mostActive2 = stN |> List.sortByDescending (fun m -> m.Activity) |> List.take 2
    let r = mostActive2[0].Activity * mostActive2[1].Activity
    r    
    
let solve2 (text:string) =
    let inp = text |> parse2lines

    let st0 = inp

    let numRound = 10000

    let modulo = inp |> List.map (fun m -> m.ModuloNum) |> List.map bigint |> List.reduce (*)

    let stN : Monkey list =
        [0..numRound-1]
        |> List.fold (
            fun (sti:Monkey list) _ ->
                [0..sti.Length-1]
                |> List.fold (
                    fun st i ->
                        let m:Monkey = st[i]

                        let itemsTo =
                            m.Items
                            |> map (
                                fun item ->
                                    let item2 = m.Op item
                                    let item3 = item2 % modulo// /3L
                                    let throwTo = if item3%m.DivTest=0 then m.ToTrue else m.ToFalse
                                    item3,throwTo,i
                            )

                        let mp = st |> List.indexed |> Map.ofList
                        let mp2 : Map<int,Monkey> = 
                            itemsTo
                            |> List.fold (
                                fun imp (num,xTo,xFrom) ->
                                    let monkeyTo : Monkey = imp[xTo]
                                    let nextItems = monkeyTo.Items @ [num]
                                    let imp2 = imp |> Map.add xTo {monkeyTo with Items=nextItems}

                                    let monkeyFrom : Monkey = imp[xFrom]
                                    let imp3 = imp2 |> Map.add xFrom {monkeyFrom with Activity=monkeyFrom.Activity+1; Items=[]}
                                    imp3
                            ) mp

                        mp2.Values |> List.ofSeq
                ) sti
                    





        ) st0

    
    let mostActive2 = stN |> List.sortByDescending (fun m -> m.Activity) |> List.take 2
    let r = (bigint mostActive2[0].Activity) * (bigint mostActive2[1].Activity)
    r |> int64

//let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())


let finished = true
()