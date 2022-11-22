﻿namespace Aoc22

open System.IO
open System.Reflection

module Input =
    let rec private skipLastEmpty (lst:string list) =
        match lst with
        | [] -> []
        | [""] -> []
        | [x] -> [x]
        | head :: tail -> 
            let skippedTail = skipLastEmpty(tail)
            match head, skippedTail with
            | "", [] -> []
            | _ -> head :: skippedTail

    let f2text fpath = 
        let dir = Directory.GetParent(Assembly.GetEntryAssembly().Location).FullName
        fpath |> fun nm -> Path.Combine(dir, nm) |> File.ReadAllText
    let f2lines fpath = fpath |> File.ReadAllLines |> List.ofSeq |> skipLastEmpty
    let text2tokens (splitCh:string) (text:string) = text.Split(splitCh.ToCharArray(), System.StringSplitOptions.RemoveEmptyEntries) |> List.ofArray
    let text2tokensStr (splitStrs:string list) (text:string) = text.Split(splitStrs |> Array.ofSeq, System.StringSplitOptions.RemoveEmptyEntries) |> List.ofArray
    let text2lines (text:string) = text.Split("\r\n") |> List.ofArray |> skipLastEmpty
    let f2tokens splitCh fpath = fpath |> f2text |> text2tokens splitCh
