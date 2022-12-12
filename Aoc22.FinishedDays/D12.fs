module Aoc22.D12

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

type Point = Elevation of int | Start | End

let parseLine (ln:string) =
    ln.ToCharArray() 
    |> List.ofArray 
    |> List.map (
        function
        | 'S' -> Start
        | 'E' -> End
        | c -> Elevation(int c - (int 'a'))
    )
    // |> text2tokens "x"
    // |> text2tokensStr ["abc";"def"]

let parse2lines (text:string) = 
    text
    |> text2lines 
    // |> Input.list2groups ((=)"")
    |> List.map parseLine
    |> List.mapi (fun y ln -> ln |> List.mapi (fun x el -> (x,y),el))
    |> List.concat


let prepareMap inp =
    let s = inp |> List.find (fun (pos,z)->z=Start) |> fst
    let e = inp |> List.find (fun (pos,z)->z=End) |> fst

    
    let realPos0 = inp |> List.map (fun (pos,c) -> pos,(match c with | Start -> 0 | End -> (int 'z' - int 'a') | Elevation(x) -> x))
    let maxX = realPos0 |> List.map fst |> List.map fst |> List.max
    let maxY = realPos0 |> List.map fst |> List.map snd |> List.max
    let boundTop = [-1..maxX+1] |> List.map (fun x -> (x,-1),Int32.MaxValue)
    let boundBot = [-1..maxX+1] |> List.map (fun x -> (x,maxY+1),Int32.MaxValue)
    let boundLeft = [0..maxY] |> List.map (fun y -> (-1,y),Int32.MaxValue)
    let boundRight = [0..maxY] |> List.map (fun y -> (maxX+1,y),Int32.MaxValue)
    let realPos = realPos0@boundTop@boundBot@boundLeft@boundRight
    let mp = realPos |> Map

    s,e,mp

let getNextPos (mp: Map<int*int,int>) (x,y) =
    let elev = mp[x,y]

    let plus = [(-1,0);(1,0);(0, -1);(0, 1)] |> seq
    plus
    |> Seq.map ((+..) (x,y))
    |> Seq.filter (fun pos2 -> mp[pos2] <= elev+1)

let solve1 (text:string) = 
    let inp = text |> parse2lines
    
    let s,e,mp = prepareMap inp

    let res = Common.Dijkstra s (getNextPos mp) ((=)e)
    let len = res |> Seq.length
    len-1

let badStartPos (mp: Map<int*int,int>) (x,y) =
    [(-1,0);(1,0);(0, -1);(0, 1)]
    |> List.map (fun m -> m +.. (x,y))
    |> List.forall (fun pos -> 
                            let v = mp[pos]
                            v<>1
    )
    
let solve2 (text:string) =
    let inp = text |> parse2lines

    let _,e,mp = prepareMap inp

    let allAs = mp |> Map.toList |> List.filter (fun (pos,a) -> a=0) |> List.map fst
    let filteredAs = allAs |> List.filter (fun aPos -> not (badStartPos mp aPos))

    let allPossible = 
        filteredAs
        |> List.map (fun s ->
            try 
                let res = Common.Dijkstra s (getNextPos mp) ((=)e)
                let len = res |> Seq.length
                len-1
            with
            | :? Exception -> Int32.MaxValue
        )

    let r = allPossible |> List.min
    r
    

//let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())


let finished = true
()