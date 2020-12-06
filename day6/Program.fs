open System.IO

let data =
    let handle s l =
        let (x, xs) = match s with 
                        | [] -> ([] ,[])
                        | x::xs -> (x, xs)
        if l = "" then []::x::xs else (l::x)::xs
    File.ReadAllLines "../data/input6.txt" |> Array.fold handle []

let countUnique l = List.map Set.ofSeq l |> Set.unionMany |> Set.count
printfn $"{Seq.sumBy countUnique data}"

let countCommon l = List.map Set.ofSeq l |> Set.intersectMany |> Set.count
printfn $"{Seq.sumBy countCommon data}"