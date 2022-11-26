﻿module Aoc22.FinishedDays.Test.T_Aoc18_D24
open Aoc22.Practice_Aoc18_D24

open Aoc22.Input
open NUnit.Framework
open System

[<SetUp>]
let Setup () =
    ()

let basePath = "Practice"
let prependFolder fname = basePath + "\\" + fname
    
[<TestCase("input-TEST.txt", 28976)>] 
let Test1 (fn : string, res: int) = 
    let input = fn |> prependFolder |> f2text
    let sln1 = solve1 input
    Assert.That(sln1, Is.EqualTo res)
    
[<TestCase("input-TEST.txt", 3534)>] 
let Test2 (fn : string, res: int) = 
    let input = fn |> prependFolder |> f2text
    let sln2 = solve2 input
    Assert.That(sln2, Is.EqualTo res)
