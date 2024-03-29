﻿namespace Aoc22

open System.IO
open System.Reflection

module Input =
    let rec private skipFirstEmpty (lst:string list) =
        match lst with
        | [] -> []
        | ""::tail -> skipFirstEmpty tail
        | x -> x

    let f2text fpath = 
        let dir = Directory.GetParent(Assembly.GetEntryAssembly().Location).FullName
        fpath |> fun nm -> Path.Combine(dir, nm) |> File.ReadAllText
    let f2lines fpath = fpath |> File.ReadAllLines |> List.ofSeq |> List.rev |> skipFirstEmpty |> List.rev
    let text2tokens (splitCh:string) (text:string) = text.Split(splitCh.ToCharArray(), System.StringSplitOptions.RemoveEmptyEntries) |> List.ofArray
    let text2tokensStr (splitStrs:string list) (text:string) = text.Split(splitStrs |> Array.ofSeq, System.StringSplitOptions.RemoveEmptyEntries) |> List.ofArray
    let text2lines (text:string) = text.Split("\r\n") |> List.ofArray |> List.rev |> skipFirstEmpty |> List.rev
    let f2tokens splitCh fpath = fpath |> f2text |> text2tokens splitCh
    let list2groups (isDelimiterLine: 'T -> bool) (lst : 'T list) = 
        let state0 : 'T list list = [[]]
        lst
        |> List.fold (fun (x::y) item -> if (isDelimiterLine item) then []::x::y else (item::x)::y) state0
        |> List.map List.rev
        |> List.rev
