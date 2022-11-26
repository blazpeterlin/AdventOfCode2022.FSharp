module Aoc22.Practice_Aoc18_D24

open Aoc22
open Input
open TextCopy
open System.Collections.Generic
open Aoc22.Operators
open Microsoft.FSharp.Core.Operators.Checked
open System.Text.RegularExpressions
open System

let input =  "input.txt" |> f2text

type Side = IMM | INF
type U = { Id: int; Num: int; HP: int; Immunities: string list; Weaknesses: string list; Attack: int; AttackType: string; Initiative: int; Side: Side }
type LN = LNU of U | LNSide of Side

let parseLine (lnId:int) (ln:string) : LN =
    if ln = "Immune System:" then LNSide(IMM) else
    if ln = "Infection:" then LNSide(INF) else

    let tkns = 
        ln
        // |> text2tokens "x"
        |> text2tokensStr [" units each with ";" hit points "; "with an attack that does"; " damage at initiative "]

    
    let immuneTo, weakTo = 
        //let m = Regex.Match(ln, @"\((weak to ((?<weakness>\w*)(,\s)?)+)?(;\s)?(immune to ((?<immunity>\w*)(,\s)?)+)?\)")
            
        //let wk = m.Groups |> Seq.tryFind (fun gr -> gr.Name="weakness")
        //let imm = m.Groups |> Seq.tryFind (fun gr -> gr.Name="immunity")

        //let getWords (grpO:Group option) =
        //        match grpO with 
        //        | None -> []
        //        | Some grp -> grp.Captures |> Seq.map (fun c -> c.Value) |> List.ofSeq |> List.except [""]

        //getWords imm, getWords wk
        let m = Regex.Match(ln, @"\((?<x>.*)\)")
        let grps = m.Groups["x"].Value |> text2tokens ";" |> map (fun s ->s.Trim()) |> List.sortBy id //m.Groups |> List.ofSeq |> map (fun x -> x.Value)

        let getWords (str: string) =
            str |> text2tokens ", " |> List.skip 2

        if grps.Length=0 then [],[] else
        let immuneToR = if grps[0].StartsWith("immune to") then getWords grps[0] else []
        let weakToR = if grps[grps.Length-1].StartsWith("weak to") then getWords grps[grps.Length-1] else []
        immuneToR,weakToR
        
        
    let len = tkns.Length

    let tail = tkns[tkns.Length-2] |> text2tokens " "

    let u = {
        Num = int tkns[0];
        HP = int tkns[1];
        Attack = int tail[0];
        AttackType = tail[1];
        Initiative = int tkns[len-1];
        Immunities = immuneTo;
        Weaknesses = weakTo;
        Side=IMM;
        Id=lnId;
    }
    LNU(u)


let parse2lines (text:string) = 
    let r = 
        text
        |> text2lines 
        |> filter (fun ln -> ln <> "")
        |> List.mapi parseLine

    let imms = r |> List.skip 1 |> List.takeWhile(fun elt -> elt <> LNSide(INF)) |> List.choose (function | LNU(u) -> Some {u with Side=IMM} | _ -> None)
    let infs = r |> List.skipWhile (fun elt -> elt<>LNSide(INF)) |> List.skip 1 |> List.choose (function | LNU(u) -> Some {u with Side=INF} | _ -> None)

    imms, infs

let power (u: U) = u.Attack * u.Num
let potentialDamageToUnit (attacker:U, victim: U) : int =
    let baseDmg = power attacker
    let totalDmg = 
        if baseDmg <= 0 then 0 else
        if victim.Immunities |> List.contains attacker.AttackType then 0 else
        if victim.Weaknesses |> List.contains attacker.AttackType then baseDmg*2
        else baseDmg
    totalDmg

let dealDamage(attacker:U, victim: U) : U =
    let totalDmg = potentialDamageToUnit (attacker,victim)
    let actualDmgedUnits = totalDmg / victim.HP
    {victim with Num = victim.Num - actualDmgedUnits }

let oneFight (imms : U list, infs : U list) =
    let allUnits = List.concat [imms;infs]
    let unitsByOrderOfAttack =
        allUnits
        |> List.sortByDescending (fun u -> power u, u.Initiative)

    let st0 = imms, infs, []
    let (_,_,attacksRev) = 
        unitsByOrderOfAttack
        |> fold (
            fun (immsI, infsI, output) unit -> 
                let opponents : U list = match unit.Side with | INF -> immsI | IMM -> infsI
                if opponents.Length = 0 then immsI,infsI,output else
                let opponent = opponents |> List.sortByDescending (fun opponent -> (potentialDamageToUnit (unit,opponent)),power opponent,opponent.Initiative) |>List.head
                if potentialDamageToUnit (unit,opponent)=0 then immsI,infsI,output else
                match unit.Side with | INF -> (immsI |> List.except [opponent]),infsI,(unit,opponent)::output | IMM -> immsI,(infsI |> List.except [opponent]),(unit,opponent)::output
        ) st0
    let attacks = attacksRev |> List.sortByDescending (fun (attacker,_) ->attacker.Initiative)

    let allUnits : Map<int, U> = List.append imms infs |> map (fun unit -> unit.Id, unit) |> Map.ofList
    
    let finalAllUnits : Map<int, U> = 
        attacks
        |> List.fold (fun units (attacker,victim) -> 
            let realAttacker = units[attacker.Id]
            let realVictim = units[victim.Id]
            //let realAttacker,realVictim = attacker,victim

            let finalVictim = dealDamage (realAttacker, realVictim)
        
            let nextAllUnits = units |> Map.add victim.Id finalVictim
            nextAllUnits
        ) allUnits

    let survivingList = finalAllUnits.Values |> Seq.filter(fun u -> u.Num > 0)
    let allUnitsImm: U list = survivingList |> Seq.filter(fun x -> x.Side=IMM) |> List.ofSeq
    let allUnitsInf: U list = survivingList |> Seq.filter(fun x -> x.Side=INF) |> List.ofSeq
    allUnitsImm,allUnitsInf




let solve1 (text:string) = 
    let imms0,infs0 = text |> parse2lines
    let immsN,infsN = 
        (imms0,infs0)
        |> Common.unfold (fun (immsI, infsI) -> 
            if immsI.Length=0 || infsI.Length=0 then None else
            let immsNext,infsNext = oneFight (immsI,infsI)
            //if immsNext=immsI && infsNext=infsI then None else
            (immsNext, infsNext) |> Some
        )
        |> Seq.last
    let howManyUnits = List.append immsN infsN |> map (fun x -> x.Num) |> List.sum
    howManyUnits
    
let solve2 (text:string) =
    let imms0,infs0 = text |> parse2lines

    Seq.initInfinite id
    |> Seq.choose (fun boost -> 
        let immsN,infsN = 
            let boostedImms0 = imms0 |> map (fun imm -> {imm with Attack = imm.Attack + boost})
            (boostedImms0,infs0)
            |> Common.unfold (fun (immsI, infsI) -> 
                if immsI.Length=0 || infsI.Length=0 then None else
                let immsNext,infsNext = oneFight (immsI,infsI)
                if immsNext=immsI && infsNext=infsI then None else
                (immsNext, infsNext) |> Some
            )
            |> Seq.last
        if infsN.Length > 0 then None else 
        let howManyUnits = immsN |> map (fun x -> x.Num) |> List.sum
        Some howManyUnits
    )
    |> Seq.head

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())

let finished = true
()