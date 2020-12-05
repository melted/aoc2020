open System.IO

let seatId str = Seq.fold (fun s c -> 2*s + if c = 'B' || c = 'R' then 1 else 0) 0 str

let data = File.ReadAllLines "../data/input5.txt" |> Array.map seatId
let res = Array.sort data |> Seq.pairwise |> Seq.find (fun (a, b) -> b-a > 1) |> fst |> (+) 1 

printfn $"{Seq.max data}"
printfn $"{res}"