namespace Aoc22

module Operators =
    let (+..) (x0,y0) (x1,y1) = (x0+x1,y0+y1)
    let (+...) (x0,y0,z0) (x1,y1,z1) = (x0+x1,y0+y1,z0+z1)
    let (-..) (x0,y0) (x1,y1) = (x0-x1,y0-y1)
    let (-...) (x0,y0,z0) (x1,y1,z1) = (x0-x1,y0-y1,z0-z1)
    

    let inline map mapping list = List.map mapping list
    let inline filter predicate list = List.filter predicate list
    let inline sum list = List.sum list
    let inline ofArray arr = List.ofArray arr
    let inline ofSeq sq = List.ofSeq sq
    let inline distinct list = List.distinct list
    let inline length list = List.length list
    let inline windowed windowSize list = List.windowed windowSize list
    let inline fold (folder:'State -> 'T -> 'State) (state: 'State) (list: 'T list) = List.fold folder state list
    let inline head list = List.head list
