
open System.Collections.Generic

let data = [6; 4; 12; 1; 20; 0; 16]

let play target start =
    let dict = Dictionary ()
    let init = Seq.zip start [1..List.length start-1]
    for (k, t) in init do dict.[k] <- t
    let rec worker i last =
        if i = target
            then last
            else
                let next = if dict.ContainsKey last then i - dict.[last] else 0
                dict.[last] <- i
                worker (i+1) next
    worker (List.length start) (List.last start)

let res = play 2020 data
printfn $"{res}"

let res2 = play 30000000 data
printfn $"{res2}"