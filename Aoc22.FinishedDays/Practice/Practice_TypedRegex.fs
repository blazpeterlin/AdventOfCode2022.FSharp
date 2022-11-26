module Aoc22.Practice_TypedRegex

open Aoc22
open Input
open TextCopy
open System.Collections.Generic
open Aoc22.Operators
open System.Text.RegularExpressions
open FSharp.Text.RegexProvider
open FSharp.Text.RegexExtensions


type ParenRegex = Regex< @"\((?<num>\d*)\)">
let mathExpr = @"1+(2+(3))"
let simplifiedMathExpr = ParenRegex().TypedReplace(mathExpr, fun m -> m.num.Value)


type PhoneRegex = Regex< @"(?<AreaCode>^\d{3})-(?<PhoneNumber>\d{3}-\d{4}$)" >
let areaCode = PhoneRegex().TypedMatch("425-123-2345").AreaCode.Value

