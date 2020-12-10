open System.IO

let indata = File.ReadAllLines "../data/input10.txt" |> Array.map int |> Array.sort
let data = Array.append indata [| 3 + Array.last indata |]

let count (a, b, last) next = 
    match next-last with
       | 1 -> (a + 1, b, next)
       | 3 -> (a, b + 1, next)
       | _ -> (a, b, next) 

let (o, t, _) = Array.fold count (0,0,0) data

printfn $"{o*t}"

let countWays target =
    let points = Set.ofArray data
    let mutable known = Map.empty
    let rec worker from target =
        let check n =
            if Set.contains (from + n) points
                then worker (from + n) target
                else 0L
        if Map.containsKey from known 
            then known.[from]
            else if from = target
                then 1L
                else
                    let w = Seq.sumBy check [1;2;3]
                    known <- Map.add from w known
                    w
    worker 0 target

let ways = countWays (Array.last data)

printfn $"{ways}"