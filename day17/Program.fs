open System.IO

let neighbors (x,y,z) = seq { for dx in -1..1 do
                                for dy in -1..1 do
                                    for dz in -1..1 do
                                        if dx <> 0 || dy <> 0 || dz <> 0 
                                            then (x+dx, y+dy, z+dz) }

let parseMap strs =
    let setify y str = Seq.mapi (fun x c -> if c = '#' 
                                                then Set.singleton (x, y, 0)
                                                else Set.empty) str 
                            |> Set.unionMany 
    Array.mapi setify strs |> Set.unionMany

let data = File.ReadAllLines "../data/input17.txt" 
            |> parseMap

let step active nb = 
    let census = 
        let update v = match v with
                        | None -> Some 1
                        | Some x -> Some (x+1)
        let addp c p = Map.change p update c
        Seq.fold addp Map.empty (Seq.collect nb active)
    let alive p c = c = 3 || (c = 2 && Set.contains p active) 
    Map.filter alive census |> Map.toSeq |> Seq.map fst |> Set.ofSeq

let rec run i conf nb =
    if i = 0 then conf else run (i-1) (step conf nb) nb

let res = run 6 data neighbors

printfn $"{Set.count res}"

let neighbors4 (x,y,z,w) = seq { for dx in -1..1 do
                                    for dy in -1..1 do
                                        for dz in -1..1 do
                                            for dw in -1..1 do
                                                if dx <> 0 || dy <> 0 || dz <> 0 || dw <> 0 
                                                    then (x+dx, y+dy, z+dz, w+dw) }

let data4 = Set.map (fun (x,y,z) -> (x,y,z,0)) data

let res2 = run 6 data4 neighbors4

printfn $"{Set.count res2}"