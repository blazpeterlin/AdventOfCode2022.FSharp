module Aoc22.ActiveDay.Test

open NUnit.Framework
open Aoc22.ActiveDay
open TextCopy
open System.Collections.Generic
open Aoc22.Operators
open Aoc22.Input
open System

[<TestCase("input-TEST.txt", 0)>]
let TestSln1 (fn: string, res: int) =
    let input = fn |> f2text
    let sln1 = solve1 input
    Console.WriteLine(sln1)
    Assert.That(sln1, Is.EqualTo res)
    
[<TestCase("input-TEST.txt", 0)>]
let TestSln2 (fn: string, res: int) =
    let input = fn |> f2text
    let sln2 = solve2 input
    Console.WriteLine(sln2)
    Assert.That(sln2, Is.EqualTo res)
