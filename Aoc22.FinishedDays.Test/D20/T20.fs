module Aoc22.FinishedDays.Test.T20
open Aoc22.D20

open Aoc22.Input
open NUnit.Framework
open System

[<SetUp>]
let Setup () =
    ()

let basePath = System.Reflection.MethodInfo.GetCurrentMethod().DeclaringType.Name.Replace("T","D")
let prependFolder fname = basePath + "\\" + fname
    
[<TestCase("input-TEST.txt", 13883)>] 
let Test1 (fn : string, res: int) = 
    let input = fn |> prependFolder |> f2text
    let sln1 = solve1 input
    Assert.That(sln1, Is.EqualTo res)
    
[<TestCase("input-TEST.txt", 19185967576920L)>] 
let Test2 (fn : string, res: int64) = 
    let input = fn |> prependFolder |> f2text
    let sln2 = solve2 input
    Assert.That(sln2, Is.EqualTo res)
