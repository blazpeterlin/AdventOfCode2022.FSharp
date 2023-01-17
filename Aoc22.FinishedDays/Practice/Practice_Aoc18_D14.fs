module Aoc22.Practice_Aoc18_D14

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

let flip (x,y) = y,x

let parseLine (ln:string) =
    ln
    |> text2tokens " =>, "
    |> fun xs ->
        let last = xs |> List.rev |> List.take 2 |> List.rev |> fun xs -> (int64 xs[0], xs[1]) |> flip
        let body = xs |> List.rev |> List.skip 2 |> List.rev |> List.chunkBySize 2 |> List.map (fun chnk -> int64 chnk[0], chnk[1]) |> List.map flip |> Map.ofList
        body,last

let parse2lines (text:string) = 
    text
    |> text2lines 
    // |> Input.list2groups ((=)"")
    |> List.map parseLine

let isCleared (st:Map<string,int64>) =
    st |> Map.forall (fun k v -> k="ORE" || v<=0L)

let getNext (inp: (Map<string,int64> * (string*int64)) list) (st:Map<string,int64>) =
    let origOre = st["ORE"]
    inp
    |> Seq.choose (
        fun (ingredients,(reqIng,reqNum)) -> 
            if st.ContainsKey reqIng 
                //&& st[reqIng] >= reqNum
                && st[reqIng] > 0
            then 
                let factor = Math.Ceiling((Decimal st[reqIng]) / (Decimal reqNum)) |> int64
                ingredients
                |> Map.fold (
                    fun (sti:Map<string,int64>) k v -> 
                        if sti.ContainsKey(k) 
                        then sti |> Map.add k (sti[k]+v*factor)
                        else sti |> Map.add k (v*factor)
                ) (st |> Map.add reqIng (st[reqIng]-reqNum*factor))
                |> Some
            else None
    )
    //|> Seq.filter (fun newMap -> newMap |> Map.toSeq |> Seq.map snd |> Seq.forall (fun x -> x<=1000000L))
    |> Seq.map (fun newMap -> 
          let cost = newMap["ORE"] - origOre
          newMap,0)
    |> Seq.truncate 1

let solve1 (text:string) = 
    let inp = text |> parse2lines

    let st0 = ["FUEL",1L;"ORE",0L] |>Map.ofList

    let stn = Common.AStarSeq st0 (fun x -> 0) isCleared (getNext inp)  |> Seq.head |> fst |> List.head

    stn["ORE"]

type Part2State = { enough:int64; tooFar:int64 option; }
    
let solve2 (text:string) =
    let inp = text |> parse2lines


    //let startFuel = 7659732L


    let needsTooMuchOre startFuel =
        let st0 = ["FUEL",startFuel;"ORE",0L] |>Map.ofList
        let stn = Common.AStarSeq st0 (fun x -> 0) isCleared (getNext inp)  |> Seq.head |> fst |> List.head
        let tgt = 1000000000000L
        stn["ORE"] > tgt

    let enough0 = 1L
    let tooFar0 = None

    let r =
        { enough=enough0; tooFar=tooFar0; }
        |> Seq.unfold (
            fun st ->
                if st.tooFar <> None && st.tooFar.Value - st.enough = 1 then None else
                let nextCheckOfFuel = 
                    match st.tooFar with
                    | None -> st.enough * 2L
                    | Some v -> st.enough + (v-st.enough)/2L

                let isTooMuchOre = needsTooMuchOre nextCheckOfFuel
        
                let innerR =
                    (
                        if isTooMuchOre 
                        then { enough=st.enough; tooFar=Some nextCheckOfFuel; }
                        else { enough=nextCheckOfFuel; tooFar=st.tooFar; }
                    )
                Some(innerR, innerR)
        )
        |> Seq.last
    r.enough


//let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())


let finished = true
()