namespace Aoc22

open System.Collections.Generic
open System

module Common =
    let manhattan (x0,y0) (x1,y1) = abs(x1-x0)+abs(y1-y0)

    let listGroupByKeyVal (projKey:'T->'Key) (projVal:'T->'Val) (lst:'T list) : ('Key * 'Val list) list =
        lst |> List.groupBy projKey |> List.map (fun (x,lst) -> x,lst |> List.map projVal)
    let seqGroupByKeyVal (projKey:'T->'Key) (projVal:'T->'Val) (lst:'T seq) : ('Key * 'Val seq) seq =
        lst |> Seq.groupBy projKey |> Seq.map (fun (x,lst) -> x,lst |> Seq.map projVal)

    [<System.Diagnostics.DebuggerStepThrough>]
    let rec unfold f state = 
      seq {
        match f state with
        | Some x ->
          yield x
          yield! unfold f x
        | None -> ()
      }
    let seqPop (s:'T seq) : 'T =
        let enumerator = s.GetEnumerator()
        
        enumerator.MoveNext() |> ignore
        let head = enumerator.Current
        
        head

    
    let Dijkstra (pos0:'a) (getNextPos:'a->'a seq) (isFinish:'a->bool) =
        let pq = PriorityQueue() // mutable
        pq.Enqueue((pos0,[pos0]), 0)
        let visited0 = [] |> HashSet // mutable
        let res =
            visited0
            |> Seq.unfold (fun v ->
                let currPos,history = pq.Dequeue()
                let reportPos = if isFinish currPos then Some(history) else None
                if v.Add currPos then
                    let allNextPos = currPos |> getNextPos |> Seq.filter (v.Contains >> not)
                    pq.EnqueueRange (allNextPos |> Seq.map (fun pos -> (pos,pos::history),history.Length))
                Some((v, reportPos), v)
            )
            |> Seq.choose snd
            |> Seq.head
        res
    
    let DijkstraExistsAndGetVisited (pos0:'a) (getNextPos:'a->'a seq) (isFinish:'a->bool) =
        let pq = PriorityQueue() // mutable
        pq.Enqueue((pos0,[pos0]), 0)
        let visited0 = [] |> HashSet // mutable
        let res =
            visited0
            |> Seq.unfold (fun v ->
                if pq.Count=0 then None else
                let currPos,history = pq.Dequeue()
                let reportPos = if isFinish currPos then Some(history) else None
                if v.Add currPos then
                    let allNextPos = currPos |> getNextPos |> Seq.filter (v.Contains >> not)
                    pq.EnqueueRange (allNextPos |> Seq.map (fun pos -> (pos,pos::history),history.Length))
                Some((v, reportPos), v)
            )
            |> Seq.choose snd
            |> Seq.tryHead
        if res.IsSome then (true,visited0) else (false,visited0)
    
    let AStar (pos0:'a) (heuristic: 'a->int) (isFinish:'a->bool) (getNextPos:'a->('a*int) seq) =
        let pq = PriorityQueue() // mutable
        let h0 = heuristic pos0
        pq.Enqueue((pos0,[pos0],0), 0+h0)
        let v = [] |> HashSet // mutable
        let resHistory,resCost =
            seq {
                while pq.Count>0 do
                    let currPos,history,costSoFar = pq.Dequeue()
                    if v.Add currPos then
                        if isFinish currPos then yield (history,costSoFar)
                        let allNextPos = currPos |> getNextPos// |> Seq.filter (fun (pos,_) -> v.Contains pos |> not)
                        pq.EnqueueRange (allNextPos 
                                            |> Seq.map (fun (pos,cost) -> 
                                                            let h = heuristic pos
                                                            (pos,pos::history,costSoFar+cost),costSoFar+cost+h))
            }
            |> Seq.head
        resHistory,resCost
    
    let AStarFaster (pos0:'a) (heuristic: 'a->int) (isFinish:'a->bool) (getNextPos:'a->('a*int) seq) (pos2v: 'a -> 'b) =
        let pq = PriorityQueue() // mutable
        let h0 = heuristic pos0
        pq.Enqueue((pos0,[pos0],0), 0+h0)
        let v = [] |> HashSet // mutable
        let resHistory,resCost =
            seq {
                while pq.Count>0 do
                    let currPos,history,costSoFar = pq.Dequeue()
                    if v.Add (currPos |> pos2v) then
                        if isFinish currPos then yield (history,costSoFar)
                        let allNextPos = currPos |> getNextPos// |> Seq.filter (fun (pos,_) -> v.Contains pos |> not)
                        pq.EnqueueRange (allNextPos 
                                            |> Seq.map (fun (pos,cost) -> 
                                                            let h = heuristic pos
                                                            (pos,pos::[],costSoFar+cost),costSoFar+cost+h))
            }
            |> Seq.head
        resHistory,resCost
    
    let AStarFasterVBonus (pos0:'a) (heuristic: 'a->int) (isFinish:'a->bool) (getNextPos:'a->'a seq) (pos2bonus: 'a -> int) (pos2v: 'a -> 'b) =
        let pq = PriorityQueue() // mutable
        let h0 = heuristic pos0
        pq.Enqueue((pos0,[pos0],0), 0+h0)
        let v = [] |> HashSet // mutable
        let resHistory,resCost =
            seq {
                while pq.Count>0 do
                    let currPos,history,costSoFar = pq.Dequeue()
                    if v.Add (currPos |> pos2v) then
                        if isFinish currPos then yield (history,costSoFar)
                        let allNextPos = currPos |> getNextPos// |> Seq.filter (fun (pos,_) -> v.Contains pos |> not)
                        pq.EnqueueRange (allNextPos 
                                            |> Seq.map (fun (pos) -> 
                                                            let h = heuristic pos
                                                            let bonus = pos2bonus pos
                                                            (pos,pos::[],bonus),bonus+h))
            }
            |> Seq.head
        resHistory,resCost

    
    let AStarSeq (pos0:'a) (heuristic: 'a->int) (isFinish:'a->bool) (getNextPos:'a->('a*int) seq) =
        let pq = PriorityQueue() // mutable
        let h0 = heuristic pos0
        pq.Enqueue((pos0,[pos0],0), 0+h0)
        let v = [] |> HashSet // mutable
        
        let r =
            seq {
                while pq.Count>0 do
                    let currPos,history,costSoFar = pq.Dequeue()
                    if v.Add currPos then
                        if isFinish currPos then yield (history,costSoFar)
                        let allNextPos = currPos |> getNextPos// |> Seq.filter (fun (pos,_) -> v.Contains pos |> not)
                        pq.EnqueueRange (allNextPos 
                                            |> Seq.map (fun (pos,cost) -> 
                                                            let h = heuristic pos
                                                            (pos,pos::history,costSoFar+cost),costSoFar+cost+h))
            }
            |> seq
        r

    let print2d (fullCoords: (int*int) seq) =
        let printed = fullCoords |> Seq.sortBy (fun pos -> snd pos,fst pos) |> List.ofSeq
        let fstX,fstY = printed |> List.head
        let minX = printed |> List.map fst |>List.min
        let mutable prevY = fstY
        let mutable prevX = minX-1
        for (x,y) in printed do
            while prevY <> y do
                Console.WriteLine(); 
                prevX <- minX-1;
                prevY <- prevY + 1
                0 |> ignore
            
            for ix in [prevX+2 .. x] do Console.Write(' ')
            Console.Write("█")
            prevY <- y
            prevX <- x

    
    let print2dBounds (fullCoords: (int*int) seq) minX minY maxX maxY =
        let printed = fullCoords |> Seq.filter (fun (x,y) -> x >= minX && x <= maxX && y >= minY && y <= maxY) |> Seq.sortBy (fun pos -> snd pos,fst pos) |> List.ofSeq

        let fstX,fstY = printed |> List.head
        //let minX = printed |> List.map fst |>List.min
        let mutable prevY = minY
        let mutable prevX = minX-1
        for (x,y) in printed do
            while prevY <> y do
                Console.WriteLine(); 
                prevX <- minX-1;
                prevY <- prevY + 1
                0 |> ignore
            
            for ix in [prevX+2 .. x] do Console.Write(' ')
            Console.Write("█")
            prevY <- y
            prevX <- x