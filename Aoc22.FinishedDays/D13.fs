module Aoc22.D13

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

type Val = Integer of int | List of Val list

let tokenize (str:string) =
    str.ToCharArray()
    |> Seq.fold (
        fun ((dgts:string),(arr:string list)) (ch:char) ->
            if dgts.Length=0 && (ch='[' || ch=']' || ch=',') then "",(ch.ToString())::arr
            elif dgts.Length>0 && (ch='[' || ch=']' || ch=',') then "",(ch.ToString())::dgts::arr
            else dgts+ch.ToString(),arr
        
    ) ("",[])
    |> fun (lastStr,arr) ->
        if lastStr.Length>0 
        then lastStr::arr |> List.rev
        else arr |> List.rev

let rec parseLine (str:string) =

    let st0 = 0,[],""
    let tokenizedStr = tokenize str
    tokenizedStr
    |> Seq.fold (
        fun (depth,output,forInner) (tkn:string) ->
            match tkn with
            | "]" when depth <= 0 -> failwith "cannot enter here"
            | "]" when depth=1 ->
                let str = forInner+"]"
                let inner = parseLine (str.Substring(0,str.Length-1).Substring(1))
                0,output@[inner],""
            | "]" -> depth-1,output,forInner+"]"
            | "[" when depth >= 0 -> depth+1,output,(forInner+tkn)
            | _ when depth >= 1 -> depth,output,(forInner+tkn)
            | "," -> depth,output,forInner
            | _ -> depth,output@[Integer((int tkn))],""
    ) st0
    |> fun (_,lst,_) -> List(lst)

let parseLineOuter (str:string) = 
    str |> parseLine |> function | List([v]) -> v


let parse2lines (text:string) = 
    text
    |> text2lines 
    |> Input.list2groups ((=)"")
    |> List.map (fun grp -> parseLineOuter grp[0], parseLineOuter grp[1])

let rec compare (val1:Val) (val2:Val) =
    match val1,val2 with
    | Integer(i1),Integer(i2) -> if i1 < i2 then Some(true) elif i1 > i2 then Some(false) else None
    | List(vals1),List(vals2) -> 
        let minL = min vals1.Length vals2.Length
        let r = 
            List.zip (vals1 |> List.take minL) (vals2 |> List.take minL)
            |> List.tryPick (fun (v1,v2) -> compare v1 v2)
        match r with
        | Some(v) -> Some(v)
        | None -> 
            if vals1.Length < vals2.Length then Some(true)
            elif vals1.Length > vals2.Length then Some(false)
            else None
    | Integer(i1),List(vals2) -> compare (List([Integer(i1)])) (List(vals2))
    | List(vals1),Integer(i2) -> compare (List(vals1)) (List([Integer(i2)]))

let solve1 (text:string) = 
    let inp = text |> parse2lines
    let idxs = inp |> List.indexed |> List.filter (fun (idx,(v1,v2)) -> (compare v1 v2) = Some true) |> List.map fst |>List.map ((+)1)
    let r = idxs |> List.sum

    r
    
let solve2 (text:string) =
    let inp = text |> parse2lines |> List.map (fun (v1,v2) -> v1::[v2]) |>List.concat
    let decoders = List([List([Integer(2)])])::List([List([Integer(6)])])::[]
    let inpTotal = decoders@inp
    let inpSorted = inpTotal |> List.sortWith (fun d1 d2 -> match compare d1 d2 with | Some(true)-> -1| Some(false) -> 1 | None -> 0)
    let decoderIdxs = inpSorted |> List.mapi (fun idx v -> (idx+1,v)) |> List.filter(fun (ix,v) -> decoders |> List.contains v) |> List.map fst
    let r = decoderIdxs[0]*decoderIdxs[1]
    r

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())


let finished = true
()
