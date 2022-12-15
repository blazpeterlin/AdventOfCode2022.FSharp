module Aoc22.D15

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
//open Microsoft.Z3.Bool
open Microsoft.Z3.Int
//open Microsoft.Z3.Real
//open Microsoft.Z3.BitVec
//open Microsoft.Z3.Array
open Microsoft.Z3.Function
open Microsoft.Z3.Api
open Microsoft.Z3.Addons

let input =  "input.txt" |> f2text

let parseLine (ln:string) =
    ln
    // |> text2tokens "x"
    |> text2tokensStr ["Sensor at x=";", y=";": closest beacon is at x="]
    |> fun xs -> (int xs[0], int xs[1]),(int xs[2], int xs[3])

let parse2lines (text:string) = 
    text
    |> text2lines 
    // |> Input.list2groups ((=)"")
    |> List.map parseLine

let solve1 (text:string) = 
    let inp = text |> parse2lines

    let allPos =  (inp |> List.map fst)@(inp |> List.map snd)
    let allPosSet = allPos |> Set
    let minX = allPos |> List.map fst |> List.min
    let maxX = allPos |> List.map fst |> List.max
    let minY = allPos |> List.map snd |> List.min
    let maxY = allPos |> List.map snd |> List.max

    let sensorsWithDist = inp |> List.map (fun (s, b) -> s, (Common.manhattan s b))

    let largestD = sensorsWithDist |> List.map snd |> List.max

    let susy = 2000000
    //let susy = 10
    let r =
        [minX - largestD .. maxX + largestD]
        |> List.map (fun x -> (x,susy))
        |> List.filter (fun pos -> sensorsWithDist |> List.tryFind (fun (s,d) -> Common.manhattan s pos <= d) |> function | None -> false | Some(sensor) -> true)
        |> List.filter (fun pos -> (allPosSet.Contains pos |> not))
        |> List.length

    r
    
let solve2 (text:string) =
    let inp = text |> parse2lines

    let allPos =  (inp |> List.map fst)@(inp |> List.map snd)

    let sensorsWithDist = inp |> List.map (fun (s, b) -> s, (Common.manhattan s b))

    let bMinCoord=IntVal 0
    let bMaxCoord=IntVal 4000000

    
    let opt = Opt()

    let dX = Int "x"
    let dY = Int "y"
    let dZ = Int "z"

    opt.Add(dX >=. bMinCoord)
    opt.Add(dY >=. bMinCoord)
    opt.Add(dX <=. bMaxCoord)
    opt.Add(dY <=. bMaxCoord)

    opt.Add (dZ =. dX * (IntVal 4000000) + dY)

    let ZERO = IntVal 0

    //let absFn (x:Int) = IIF_Int (x <=. ZERO, ZERO - x, x)
    let absFn (x:Int) = x <=. ZERO ??> ZERO - x --> x

    for ((sx,sy),sd) in sensorsWithDist do
        let dv = IntVal sd
        opt.Add(absFn(dX - (IntVal sx)) + absFn(dY - (IntVal sy)) >. dv)

    Gs.overrideContext(Dictionary<string,string>())

    opt.Maximize(dZ)
    opt.CheckOrFail()
    let resZ = opt.Eval dZ
    let resX = opt.Eval dX
    let resY = opt.Eval dY

    let res = int64 (resZ.ToString())
    res
    

//let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())


let finished = true
()