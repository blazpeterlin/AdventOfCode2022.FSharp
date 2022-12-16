module Aoc22.D16

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

let parseLine (ln:string) =
    ln
    // |> text2tokens "x"
    |> text2tokensStr ["Valve ";" has flow rate=";"; tunnel leads to ";"; tunnels lead to ";"valves ";"valve "]
    |> fun xs ->
        let valve=xs[0]
        let flow=int xs[1]
        let leadsTo=xs[2] |> text2tokens ", "
        valve,flow,leadsTo

let parse2lines (text:string) = 
    text
    |> text2lines 
    // |> Input.list2groups ((=)"")
    |> List.map parseLine

let solve1 (text:string) = 
    let inp = text |> parse2lines

    let mapFlowRate = inp |> List.map (fun (v,fr,lt) -> v,fr) |> Map.ofList
    let mapLeadsTo = inp |> List.map (fun (v,fr,lt) -> v,lt) |> Map.ofList

    let totalMinutes=30
    let s0 = "AA",([] |> Set),totalMinutes

    let pressuredInp = mapFlowRate |> Map.toList |> List.filter (fun (x,c) -> c<>0)

    
    let artificialAdd = 1000

    let getNextPos ((whereAt: string), (st:string Set), (minutesLeft: int)) : seq<(string * Set<string> * int) * int> =
        let test = if minutesLeft=5 && whereAt="EE" && (st.Contains("EE") |> not) && st.Count=5 then 0 else 0
        

        if minutesLeft=0 then Seq.empty else
        if st.Count=pressuredInp.Length then [(whereAt,st,minutesLeft-1),artificialAdd+0] |> seq else


        let candidates = mapLeadsTo[whereAt]

        let pressure = mapFlowRate[whereAt]
        let s1 = 
            if st.Contains whereAt || pressure = 0
            then Seq.empty
            else
                
                let cost = pressure*(minutesLeft-1)
                [ (whereAt,st |> Set.add whereAt,minutesLeft-1),artificialAdd-cost ] |>seq
        let s2 = 
            seq {
                for c in candidates do
                    yield (c,st,minutesLeft-1),artificialAdd+0
            }

        let r =s1 |> Seq.append s2
        r


    let (finalSt),cost =
        Common.AStarSeq s0 (fun ((x:string),(st),ml) -> 0) (fun ((x:string),st,ml) -> ml=0) getNextPos
        |> Seq.minBy snd
        |> fun (st,x) -> st, -x

    let realCost = cost + totalMinutes * artificialAdd

    realCost
    
let solve2 (text:string) =
    let inp = text |> parse2lines

    let mapFlowRate = inp |> List.map (fun (v,fr,lt) -> v,fr) |> Map.ofList
    let mapLeadsTo = inp |> List.map (fun (v,fr,lt) -> v,lt) |> Map.ofList

    let totalMinutes=26
    let s0 = (("AA","AA"),(None,None),true),([] |> Set),totalMinutes

    let pressuredInp = mapFlowRate |> Map.toList |> List.filter (fun (x,c) -> c<>0) |> List.map fst

    let artificialAdd = 1000

    let dist1 = mapLeadsTo |> Map.toList |> List.map (fun (xfrom,xto) -> xto |> List.map(fun yto -> xfrom,yto)) |> List.concat |> List.map (fun (xFrom,xTo) -> (xFrom,xTo),1) |> Map.ofList

    
    let distAll =
        dist1
        |> Seq.unfold (
            fun mp ->
                let maxDistSoFar = mp |> Map.toList |> List.map snd |> List.max
                let mappingsMaxDist = mp |> Map.toList |> List.filter (fun (pos,d) -> d=maxDistSoFar) |> List.map fst
                let mpNext = 
                    mappingsMaxDist
                    |> List.fold (
                        fun (mpi:Map<string*string,int>) (xFrom,xTo) ->
                            let newMaps =
                                mapLeadsTo[xTo] 
                                |> List.filter(fun yTo -> mpi.ContainsKey(xFrom,yTo) |> not)
                            newMaps
                            |> List.fold (
                                fun m yTo ->
                                    m.Add ((xFrom,yTo),maxDistSoFar+1)
                            ) mpi
                    ) mp
                if mpNext.Count = mp.Count then None else Some(mpNext,mpNext)

        ) |> Seq.last

    let minDist target source = distAll[source,target]


    let getNextPos (((whereYouAt: string, whereElephantAt: string), (yourGoal: string option, elephantGoal: string option), yourTurn: bool), (st:string Set), (minutesLeft: int)) 
                                        : seq<(((string*string)*((string option) * (string option))*bool) * Set<string> * int) * int> =

        let whereAt, goal, oppositeGoal = if yourTurn then whereYouAt,yourGoal,elephantGoal else whereElephantAt,elephantGoal,yourGoal
        
        let generateNextWhereAt wa = if yourTurn then wa,whereElephantAt else whereYouAt,wa
        let nextMinutesLeft = if yourTurn then minutesLeft else minutesLeft-1

        if minutesLeft=0 then Seq.empty else
        if st.Count=pressuredInp.Length then [(((generateNextWhereAt whereAt),(yourGoal,elephantGoal),not yourTurn),st,nextMinutesLeft),artificialAdd+0] |> seq else



        let pressure = mapFlowRate[whereAt]
        let s1 = 
            if st.Contains whereAt || pressure = 0 || (not (goal.IsSome && goal.Value = whereAt))
            then Seq.empty
            else
                
                let cost = pressure*(minutesLeft-1)

                [ (((generateNextWhereAt whereAt),((if yourTurn then None else yourGoal),(if not yourTurn then None else elephantGoal)),not yourTurn),st |> Set.add whereAt,nextMinutesLeft),artificialAdd-cost ] |>seq


        let candidates = mapLeadsTo[whereAt]
        let actualGoals = 
            match goal with
            | Some g -> [g]
            | None -> 
                let gs = pressuredInp |> List.filter (st.Contains >> not) |>List.filter (fun valve -> not(oppositeGoal.IsSome && oppositeGoal.Value=valve) )
                if gs.Length>0 then gs
                else pressuredInp |> List.take 1 // whatever, just pick a random goal
        let s2 = 
            seq {
                for g in actualGoals do 
                    let smallestDist = candidates |> List.map (fun c -> minDist c g) |> List.min
                    let nicestCandidates = candidates |> List.filter (fun c -> (minDist c g) = smallestDist)
                    for c in nicestCandidates do 
                        yield (((generateNextWhereAt c),((if yourTurn then Some g else yourGoal),if not yourTurn then Some g else elephantGoal),not yourTurn),st,nextMinutesLeft),artificialAdd+0
            }

        let r =s1 |> Seq.append s2
        r

    let pos2v (((whereYouAt: string, whereElephantAt: string), (yourGoal: string option, elephantGoal: string option), yourTurn: bool), (st:string Set), (minutesLeft: int))  =
        whereYouAt,whereElephantAt,yourGoal,elephantGoal,yourTurn,minutesLeft

    let (finalSt),cost =
        Common.AStarFaster s0 (fun (_,st,ml) -> 0) (fun (_,st,ml) -> ml=0) getNextPos pos2v
        //|> Seq.minBy snd
        |> fun (st,x) -> st, -x

    let realCost = cost + totalMinutes * artificialAdd * 2

    realCost

//let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())


let finished = true
()