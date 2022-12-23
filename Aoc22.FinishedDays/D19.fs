module Aoc22.D19

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

type Blueprint = { Id: int; OreR_Ore: int; ClayR_Ore: int; ObsR_Ore: int; ObsR_Clay: int; GeoR_Ore: int; GeoR_Obs: int; }

let parseLine (ln:string) =
    ln
    // |> text2tokens "x"
    |> text2tokensStr ["Blueprint ";": Each ore robot costs ";" ore. Each clay robot costs ";" ore. Each obsidian robot costs ";" ore and ";" clay. Each geode robot costs ";" ore and ";" obsidian."]
    |> fun xs ->
        { Id=int xs[0]; OreR_Ore = int xs[1]; ClayR_Ore = int xs[2]; ObsR_Ore = int xs[3]; ObsR_Clay = int xs[4]; GeoR_Ore = int xs[5]; GeoR_Obs = int xs[6]; }
        
type StateCore = { Ore: int; Clay: int; Obs: int; Geo: int; OreR: int; ClayR: int; ObsR: int; GeoR: int; }
type State = { SC:StateCore; MinutesLeft: int; }

let parse2lines (text:string) = 
    text
    |> text2lines 
    // |> Input.list2groups ((=)"")
    |> List.map parseLine

let getNextPos (bp: Blueprint) (st:State) : (State) seq =
    let sc = st.SC
    let minedSc = { sc with 
                Ore=sc.Ore+sc.OreR;
                Clay=sc.Clay+sc.ClayR;
                Obs=sc.Obs+sc.ObsR;
                Geo=sc.Geo+sc.GeoR;
            }
    let minedSt = { 
        st with 
            SC = minedSc;
            MinutesLeft=st.MinutesLeft - 1;
    }

    let maxOre = [bp.ClayR_Ore;bp.OreR_Ore;bp.GeoR_Ore;bp.ObsR_Ore] |> List.max |> fun mxOre -> mxOre+3

    seq { 
        //if not (sc.Ore >= bp.OreR_Ore && sc.Ore >= bp.ClayR_Ore &&  sc.Ore >= bp.ObsR_Ore && sc.Clay >= bp.ObsR_Clay && sc.Ore >= bp.GeoR_Ore && sc.Obs >= bp.GeoR_Obs )
        //    && not (sc.Ore >= bp.OreR_Ore && sc.Ore >= bp.ClayR_Ore && sc.Clay >= bp.ObsR_Clay && sc.Obs >= bp.GeoR_Obs)
        //then yield minedSt
        yield minedSt

        //if minedSt.MinutesLeft=0
        //then 0|> ignore
        //else
        if sc.Ore >= bp.OreR_Ore then yield { minedSt with SC={ minedSc with Ore=minedSc.Ore-bp.OreR_Ore; OreR=minedSc.OreR+1} } 
        if sc.Ore >= bp.ClayR_Ore then yield { minedSt with SC={ minedSc with Ore=minedSc.Ore-bp.ClayR_Ore; ClayR=minedSc.ClayR+1} }
        if sc.Ore >= bp.ObsR_Ore && sc.Clay >= bp.ObsR_Clay then yield { minedSt with SC={ minedSc with Ore=minedSc.Ore-bp.ObsR_Ore; Clay=minedSc.Clay-bp.ObsR_Clay; ObsR=minedSc.ObsR+1} } 
        if sc.Ore >= bp.GeoR_Ore && sc.Obs >= bp.GeoR_Obs then yield { minedSt with SC={ minedSc with Ore=minedSc.Ore-bp.GeoR_Ore; Obs=minedSc.Obs-bp.GeoR_Obs ; GeoR=minedSc.GeoR+1} } 
            //0|>ignore
    }
    |> Seq.map (
        fun seqst -> 
        { seqst with 
            SC= 
            { seqst.SC with 
                Ore=min maxOre seqst.SC.Ore
            }
        }
    )

    
    

let pos2v (st:State) = st.SC


let h (st:State) =
    0
    //1000 * st.MinutesLeft

let baseScore = 100000000
let geoScore = 1000

let potentialUnmadeGeosByMinutesLeft = 
    [1 .. 32]
    |> List.map (
        fun i -> 
            i, ([1 .. i] |> List.sum)
    )
    |> Map.ofList


let pos2bonus (st:State) =
    let sc = st.SC
    let r0 = baseScore - sc.Geo * geoScore

    let r = 
        if st.MinutesLeft > 1
        then
            let potentialUnmadeGeoRsYield = potentialUnmadeGeosByMinutesLeft[st.MinutesLeft-1]
            //let potentialUnmadeGeoRsYield = potentialUnmadeGeosByMinutesLeft[st.MinutesLeft/2+1]
            r0 - potentialUnmadeGeoRsYield * geoScore // - potentialGeoMakers
        else r0 - st.MinutesLeft * 1

    if st.MinutesLeft > 0 
    then r - ((st.MinutesLeft) * sc.GeoR) * geoScore
    else r


let solve1 (text:string) = 
    let inps = text |> parse2lines
    let totalMins = 24

    let st0 = { SC={Ore=0;Clay=0;Obs=0;Geo=0; OreR=1;ClayR=0;ObsR=0;GeoR=0;};  MinutesLeft=totalMins; }

    let resById =
        inps
        |> List.map (fun inp -> inp, Common.AStarFasterVBonus st0 h (fun x -> x.MinutesLeft=0) (getNextPos inp) pos2bonus pos2v)
        |> List.map (fun (inp, (rres::tail,_)) -> (inp, rres))
    let res = resById |> List.map (fun (inp,r) -> inp.Id * r.SC.Geo)
        
    let r = res |> List.sum

    r

    
let solve2 (text:string) =
    let inps = text |> parse2lines |> List.take 3
    let totalMins = 32

    let st0 = { SC={Ore=0;Clay=0;Obs=0;Geo=0; OreR=1;ClayR=0;ObsR=0;GeoR=0;};  MinutesLeft=totalMins; }

    let resById =
        inps
        |> List.map (fun inp -> inp, Common.AStarFasterVBonus st0 h (fun x -> x.MinutesLeft=0) (getNextPos inp) pos2bonus pos2v)
        |> List.map (fun (inp, (rres::tail,_)) -> (inp, rres))
    let res = resById |> List.map (fun (inp,r) -> r.SC.Geo)
        
    let r = res |> List.reduce (*)
    r

//let res1 = input |> solve1
//let res2 = input |> solve2

//ClipboardService.SetText(res1.ToString())


let finished = true
()