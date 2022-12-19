module Aoc22.ActiveDay

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

let pos2v (st:State) = st.SC


let h (st:State) =
    0
    //1000 * st.MinutesLeft

let pos2bonus (st:State) =
    let sc = st.SC
    let r0 = 100000 - sc.Geo // - st.MinutesLeft

    //let r = r0 - st.MinutesLeft * 1000
    let r = 
        if st.MinutesLeft > 1
        then
            let potentialUnmadeGeoRsYield = [2..st.MinutesLeft] |> List.map (fun anotherGeoRmins -> anotherGeoRmins-1) |> List.sum
            r0 - potentialUnmadeGeoRsYield
        else r0 - st.MinutesLeft

    if st.MinutesLeft > 0 
    then r - ((st.MinutesLeft) * sc.GeoR)
    else r
    //let r = 10000000 - st.Geo - st.MinutesLeft * st.GeoR - st.MinutesLeft
    //if r < 10000000 then 0 else 0
    //r


let solve1 (text:string) = 
    let inps = text |> parse2lines
    let totalMins = 24

    let st0 = { SC={Ore=0;Clay=0;Obs=0;Geo=0; OreR=1;ClayR=0;ObsR=0;GeoR=0;};  MinutesLeft=totalMins; }

    let resById =
        inps
        //Common.AStarFasterVBonus st0 h (fun x -> x.MinutesLeft=0) (getNextPos inp[0]) pos2bonus pos2v
        |> List.map (fun inp -> inp, Common.AStarFasterVBonus st0 h (fun x -> x.MinutesLeft=0) (getNextPos inp) pos2bonus pos2v)
        |> List.map (fun (inp, (rres::tail,_)) -> (inp, rres))
    let res = resById |> List.map (fun (inp,r) -> inp.Id * r.SC.Geo)
        


    // 1642 too low
    res |> List.sum

    
let solve2 (text:string) =
    let inps = text |> parse2lines |> List.take 3
    let totalMins = 32

    let st0 = { SC={Ore=0;Clay=0;Obs=0;Geo=0; OreR=1;ClayR=0;ObsR=0;GeoR=0;};  MinutesLeft=totalMins; }

    let resById =
        inps
        //Common.AStarFasterVBonus st0 h (fun x -> x.MinutesLeft=0) (getNextPos inp[0]) pos2bonus pos2v
        |> List.map (fun inp -> inp, Common.AStarFasterVBonus st0 h (fun x -> x.MinutesLeft=0) (getNextPos inp) pos2bonus pos2v)
        |> List.map (fun (inp, (rres::tail,_)) -> (inp, rres))
    let res = resById |> List.map (fun (inp,r) -> r.SC.Geo)
        


    // 1642 too low
    let r = res |> List.reduce (*)
    r

//let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())


let finished = true
()