
let data = [6; 4; 12; 1; 20; 0; 16]

let play target start =
    let init = Seq.zip start [1..List.length start-1] |> Map.ofSeq
    let rec worker i prev last =
        if i = target 
            then last
            else
                let next = Map.tryFind last prev |> Option.map (fun t -> i - t) |> Option.defaultValue 0
                worker (i+1) (Map.add last i prev) next
    worker (List.length start) init (List.last start)

let res = play 2020 data
printfn $"{res}"

let res2 = play 30000000 data
printfn $"{res2}"