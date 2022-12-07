module Aoc22.D07

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

//type CmdContext = CD of string | LS of string
type FileOrDir = { Dir:bool; Path:string; SelfSize:int; }

let parseLine (ln:string) =
    ln
    |> text2tokens " "
    // |> text2tokensStr ["abc";"def"]

let parse2lines (text:string) = 
    text
    |> text2lines 
    // |> Input.list2groups ((=)"")
    |> List.map parseLine

let upOneLevel currPathPrev =
    let tkns = currPathPrev |> text2tokens "/"
    let exceptLast = tkns |> List.rev |> List.skip 1 |> List.rev
    "/" + System.String.Join("/", exceptLast)

let processLine (currPathPrev : string) (filesPrev: FileOrDir list) (tkns: string list) =
    let currPath = 
        match tkns[0] with
        | "$" -> match tkns[1] with 
                    | "cd" -> match tkns[2] with
                                | "/" -> "/"
                                | ".." -> upOneLevel(currPathPrev)
                                | subDir -> currPathPrev + "/" + subDir
                    | "ls" -> currPathPrev
        | _ -> currPathPrev

        |> fun x -> x.Replace("//","/")

    if tkns[0]="$" then (currPath, filesPrev) else
    let anotherFile = 
        match tkns[0] with
        | "dir" -> { Dir=true; Path=currPath+"/"+tkns[1]; SelfSize=0; }
        | numStr ->
            let num = int numStr
            { Dir=false;Path=currPath+"/" + tkns[1]; SelfSize=num; }

    (currPath.Replace("//","/"), anotherFile::filesPrev)

let rec getAllDirs (filePath: string) : string list =
    if filePath = "/" then [] else
    let upper = upOneLevel filePath
    let inner = getAllDirs upper
    upper::inner

let getFilesAndDirsSizes (input: string) =
    let inp = input |> parse2lines
    let st0 = ("/", [])
    let fullFileState =
        inp
        |> fold (fun (dir,files) tkns ->
                let (nextDir, nextLns) = processLine dir files tkns
                nextDir,nextLns
            )
            st0
        |> snd
    let dirPaths = fullFileState |> filter (fun x -> not x.Dir) |> map (fun f -> getAllDirs f.Path) |> List.concat |> List.distinct
    let files = fullFileState |> filter (fun x -> not x.Dir)
    let dirsSizes =
        dirPaths
        |> map (fun dirPath ->
                    let inners = files |> filter (fun f -> f.Path.StartsWith(dirPath+"/"))
                    dirPath, (inners |> map (fun f -> f.SelfSize) |> List.sum)
        )
    files,dirsSizes

let solve1 (text:string) = 
    let _,dirsSizes = getFilesAndDirsSizes text

    let r = dirsSizes |> filter (fun (pth,size) ->size <= 100000) |> map snd |> List.sum

    r

    
let solve2 (text:string) =
    let files,dirsSizes = getFilesAndDirsSizes text

    let totalAtMost = 70000000 - 30000000
    let totalSize = files |> map (fun f -> f.SelfSize) |> List.sum
    let dirSizeAtLeast = totalSize - totalAtMost

    let r = dirsSizes |> filter (fun (pth,size) ->size >= dirSizeAtLeast) |> map snd |> List.sort |> List.head

    r

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())


let finished = true
()