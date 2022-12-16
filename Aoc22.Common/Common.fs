namespace Aoc22

open System.Collections.Generic

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
