open System
open System.IO

let data =
    let split (on : string) (str : string) = str.Split(on, StringSplitOptions.RemoveEmptyEntries)
    File.ReadAllText "../data/input6.txt" |> split "\n\n" |> Array.map (split "\n")

let countUnique l = Array.map Set.ofSeq l |> Set.unionMany |> Set.count
printfn $"{Seq.sumBy countUnique data}"

let countCommon l = Array.map Set.ofSeq l |> Set.intersectMany |> Set.count
printfn $"{Seq.sumBy countCommon data}"