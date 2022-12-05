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
        let pq = PriorityQueue()
        pq.Enqueue((pos0,[pos0]), 0)
        let visited0 = [] |> Set
        let res =
            visited0
            |> Seq.unfold (fun v ->
                let currPos,history = pq.Dequeue()
                let reportPos = if isFinish currPos then Some(history) else None
                let v2 = v.Add currPos
                let allNextPos = currPos |> getNextPos |> Seq.filter (v2.Contains >> not)
                pq.EnqueueRange (allNextPos |> Seq.map (fun pos -> (pos,pos::history),history.Length))
                Some((v2, reportPos), v2)
            )
            |> Seq.choose snd
            |> Seq.head
        res