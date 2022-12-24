module Aoc22.D21noZ3

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

type Expr = Number of int64 | Op of string*char*string

let parseLine (ln:string) =
    ln
    |> text2tokens ": "
    |> fun xs ->
        if xs.Length=2 then
            xs[0], Number(int64 xs[1])
        else
            xs[0], Op(xs[1],xs[2][0],xs[3])
    // |> text2tokensStr ["abc";"def"]

let parse2lines (text:string) = 
    text
    |> text2lines 
    // |> Input.list2groups ((=)"")
    |> List.map parseLine

let solve1 (text:string) = 
    let inp0 = text |> parse2lines

    let dict = ["_",0L] |> Map |> Dictionary
    let step inp =
        for (monkey,(expr:Expr)) in inp do
            if dict.ContainsKey(monkey) then 0|> ignore else
            match expr with
            | Number(i) -> dict[monkey] <- i
            | Op(m1,op,m2) ->
                if dict.ContainsKey(m1) && dict.ContainsKey(m2)
                then 
                    let v1 = dict[m1]
                    let v2 = dict[m2]
                    let num = 
                        match op with
                        | '+' -> v1+v2
                        | '-' -> v1-v2
                        | '*' -> v1*v2
                        | '/' -> v1/v2
                    dict[monkey] <- num
                    0|> ignore
                else 0|>ignore

    while not(dict.ContainsKey("root")) do
        step inp0

    let r = dict["root"]

    r
    
let solve2 (text:string) =
    let inp0 = text |> parse2lines

    let mp0 = ["_",0L] |> Map
    let stepEval mpi =
        inp0
        |> List.fold (
            fun (mp:Map<string,int64>) (monkey,(expr:Expr)) ->
                if monkey="humn" then mp else
                if mp.ContainsKey monkey then mp else
                match expr with
                | Number(i) -> mp |> Map.add monkey i
                | Op(m1,op,m2) ->
                    if mp.ContainsKey m1 && mp.ContainsKey m2
                    then 
                        let v1,v2 = mp[m1],mp[m2]
                        let num = 
                            match op with
                            | '+' -> v1+v2
                            | '-' -> v1-v2
                            | '*' -> v1*v2
                            | '/' -> v1/v2
                        mp |> Map.add monkey num
                    else mp
        ) mpi

    // eval back to starting point - we know root is an equality check
    let stepDestructure mpi =
        inp0
        |> List.fold (
            fun (mp:Map<string,int64>) (monkey,expr) ->
                if monkey="root" then 0|>ignore
                match expr with
                | Number(i) -> mp
                | Op(m1,op,m2) ->
                    if mp.ContainsKey m1 && mp.ContainsKey m2 then mp else
                    let targetKey,otherVal = if mp.ContainsKey m1 then m2,mp[m1] else m1,mp[m2]
                    if monkey="root" then mp |> Map.add targetKey otherVal else
                    match mp.TryFind monkey with
                    | None -> mp
                    | Some outerVal ->
                        match op with
                        | '+' -> mp |> Map.add targetKey (outerVal-otherVal)
                        | '*' -> mp |> Map.add targetKey (outerVal/otherVal)
                        | '-' -> mp |> Map.add targetKey (if targetKey=m1 then outerVal+otherVal else otherVal-outerVal)
                        | '/' -> mp |> Map.add targetKey (if targetKey=m1 then outerVal*otherVal else otherVal/outerVal)
        ) mpi

    let unfoldMap mpStart mpOperation =
        mpStart
        |> Seq.unfold (fun mp -> let r = mpOperation mp in Some(r,r))
        |> Seq.pairwise
        |> Seq.takeWhile(fun (mp1,mp2) -> mp1<>mp2)
        |> Seq.last
        |> snd

    let mpEval = unfoldMap mp0 stepEval

    let mpDestructured = unfoldMap mpEval stepDestructure

    let r = mpDestructured["humn"]


    r

    

//let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())


let finished = true
()