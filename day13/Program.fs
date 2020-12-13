open System.IO
open Utils.IO

let (time , depts, schedules) =
    let data = File.ReadAllLines "../data/input13.txt"
    let fields = split "," data.[1]
    let depts = Array.filter ((<>) "x") fields |> Array.map int64
    let offsets = Array.fold (fun (xs, t) s -> if s = "x" 
                                                then (xs, t+1L) 
                                                else ((int64 s, t)::xs), t+1L) 
                             ([], 0L) fields 
                             |> fst
    (int64 data.[0], depts, offsets)

let (busId, wait) = Seq.map (fun t -> (t, t - time % t)) depts |> Seq.minBy snd

printfn $"{busId*wait}"

let rec calculate sched =
    match sched with
        | [] -> (1L, 0L)
        | (t, o)::xs ->
            let (period, first) = calculate xs
            let times = Seq.initInfinite (fun i -> first + period*int64 i)
            let newFirst = Seq.find (fun x -> (x+o) % t = 0L) times
            (t*period, newFirst)

let (_, first) = calculate schedules

printfn $"{first}"