module Aoc22.D13b

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
open FParsec

let input =  "input.txt" |> f2text

type Expr = 
    | Num of int
    | Array of Expr list
    | WorkingArray of Expr list

    
let ws = spaces
let opp = new OperatorPrecedenceParser<Expr,unit,unit>()
let expr = opp.ExpressionParser
let term = 
    (pint32 |>> Num) 
    <|> (pstring "[]" |>> fun _ -> Array([]))
    <|> (between (pstring "[") (pstring "]") expr |>> function | Num(n) -> Array([Num(n)]) | Array(a) -> Array(a) |WorkingArray(a) ->Array(a)) 
opp.TermParser <- term
opp.AddOperator(InfixOperator(",", ws, 1, Associativity.Left, fun x y -> match x with | WorkingArray(lst) -> WorkingArray(lst@[y]) | v -> WorkingArray(v::y::[]) | _ -> failwith "huh"))

//let term = 
//    (pint32 |>> Num) 
//    <|> (pstring "[]" |>> fun _ -> Array([]))
//    <|> (between (pstring "[") (pstring "]") expr |>> function | Num(n) -> Array([Num(n)]) | x -> x) 
//    <|> (sepBy1 expr (pstring ",") |>> Array)
//opp.TermParser <- term

let parseLine (ln:string) =
    run expr ln |> function | Success(res,_,_) -> res | Failure(a,b,c)-> failwith "blarg"

let parse2lines (text:string) = 
    text
    |> text2lines 
    |> Input.list2groups ((=)"")
    |> List.map (fun grp -> parseLine grp[0], parseLine grp[1])


let rec compare (val1:Expr) (val2:Expr) =
    match val1,val2 with
    | Num(i1),Num(i2) -> if i1 < i2 then Some(true) elif i1 > i2 then Some(false) else None
    | Array(vals1),Array(vals2) -> 
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
    | Num(i1),Array(vals2) -> compare (Array([Num(i1)])) (Array(vals2))
    | Array(vals1),Num(i2) -> compare (Array(vals1)) (Array([Num(i2)]))

let solve1 (text:string) = 
    let inp = text |> parse2lines
    let idxs = inp |> List.indexed |> List.filter (fun (idx,(v1,v2)) -> (compare v1 v2) = Some true) |> List.map fst |>List.map ((+)1)
    let r = idxs |> List.sum

    r
    
let solve2 (text:string) =
    let inp = text |> parse2lines |> List.map (fun (v1,v2) -> v1::[v2]) |>List.concat
    let decoders = Array([Array([Num(2)])])::Array([Array([Num(6)])])::[]
    let inpTotal = decoders@inp
    let inpSorted = inpTotal |> List.sortWith (fun d1 d2 -> match compare d1 d2 with | Some(true)-> -1| Some(false) -> 1 | None -> 0)
    let decoderIdxs = inpSorted |> List.mapi (fun idx v -> (idx+1,v)) |> List.filter(fun (ix,v) -> decoders |> List.contains v) |> List.map fst
    let r = decoderIdxs[0]*decoderIdxs[1]
    r


let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res1.ToString())


let finished = true
()