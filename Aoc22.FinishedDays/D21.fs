module Aoc22.D21

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


open Microsoft.Z3
open Microsoft.Z3.Bool
open Microsoft.Z3.Int
open Microsoft.Z3.Real
open Microsoft.Z3.BitVec
open Microsoft.Z3.Array
open Microsoft.Z3.Function
open Microsoft.Z3.Api
open Microsoft.Z3.Addons

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

    let BITS = 50 // look for numbers between 0 and 2^BITS
    let zint = BitVec BITS
    let zval = BitVecVal64 BITS
    
    let opt = Opt()

    let mHUMN = zint "humn"

    for (monkey,expr) in inp0 do
        match expr with
        | Number(i) ->
            let mvar = zint monkey
            if monkey="humn" 
            then
                // var already added
                0|>ignore
            else
                let mval = zval i
                opt.Add(mvar =. mval)
                0|>ignore
        | Op(m1,op,m2) ->
            if monkey="root" 
            then
                opt.Add((zint m1) =. (zint m2))
            else
                let mvar = zint monkey
                match op with
                | '+' -> opt.Add(mvar =. (zint m1) + (zint m2))
                | '-' -> opt.Add(mvar =. (zint m1) - (zint m2))
                | '*' -> opt.Add(mvar =. (zint m1) * (zint m2))
                | '/' -> opt.Add(mvar =. (zint m1) / (zint m2))
    
    opt.Minimize(mHUMN)
    opt.CheckOrFail()
    let rZ3 = opt.Eval mHUMN
    let r = rZ3.ToString() |> int64
    




    r

    

//let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())


let finished = true
()