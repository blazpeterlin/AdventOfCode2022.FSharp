module Aoc22.Practice_Z3_MinSquared

open Aoc22
open Input
open TextCopy
open System.Collections.Generic
open Aoc22.Operators

open Microsoft.Z3
//open Microsoft.Z3.Bool
//open Microsoft.Z3.Int
open Microsoft.Z3.Real
//open Microsoft.Z3.BitVec
//open Microsoft.Z3.Array
open Microsoft.Z3.Function
open Microsoft.Z3.Api
open Microsoft.Z3.Addons

let solve =
    let opt = Opt()

    let dX = Real "x"
    //let dFn (x:Real) = (x - 1.0) * (x - 1.0)
    let dY = Real "y"
    let dZ = Real "z"
    opt.Add(dX >=. 0.0)
    opt.Add(dY >=. 0.0)
    opt.Add(dZ =. dX + dY + 1.3)

    //dict [("Z3_get_numeral_decimal_string","true")]
    Gs.overrideContext(Dictionary<string,string>())


    //let allVars = [|dX;dY|]
    //opt.AddAll(allVars)
    opt.Minimize(dZ)
    opt.CheckOrFail()
    let res = opt.Eval dZ

    //let parsedRes = Microsoft.CodeAnalysis.CSharp.SyntaxFactory.ParseExpression(res.ToString())
    res.ToString()

let finished = true
()