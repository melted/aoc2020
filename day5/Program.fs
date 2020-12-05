open System.IO

let seatId str =
    Seq.mapi (fun i c -> if c = 'B' || c = 'R' then pown 2 (9-i) else 0) str |> Seq.sum

let data = File.ReadAllLines "../data/input5.txt" |> Array.map seatId
let res = Array.sort data |> Seq.pairwise |> Seq.find (fun (a, b) -> b-a > 1) |> fst |> (+) 1 

printfn $"{Seq.max data}"
printfn $"{res}"