open System.Text.RegularExpressions
open Utils.IO

let data = ReadGroupedText "../data/input16.txt"

let tickets = Array.map (split "," >> Array.map int) data.[2].[1..]

let solve1 tickets =
    let errorVal acc x = if x < 25 || x > 973 then acc+x else acc
    Array.sumBy (Array.fold errorVal 0) tickets

printfn $"{solve1 tickets}"

let myTicket = split "," data.[1].[1] |> Array.map int

let cleaned = Array.filter (Array.forall (fun x -> x > 24 && x < 974)) tickets

let regex = Regex @"^.+: (\d+)-(\d+) or (\d+)-(\d+)$"

let parseField str =
    let m = regex.Match str
    (int m.Groups.[1].Value, int m.Groups.[2].Value, int m.Groups.[3].Value, int m.Groups.[4].Value)

let ranges = Array.map parseField data.[0]

let valid (b1, t1, b2, t2) x = (b1 <= x && x <= t1) || (b2 <= x && x <= t2)

let allValid column field  = Array.forall (valid ranges.[field]) column

let columns = Array.transpose cleaned

let alternatives column = Seq.filter (allValid column) [0..Array.length ranges - 1] |> Set.ofSeq

let possible = Array.mapi (fun i c -> (i, alternatives c)) columns |> Map.ofArray

let rec sift possible mappings =
    if Map.isEmpty possible
        then mappings |> List.map (fun (a,b) -> (Set.minElement b, a)) |> Map.ofList
        else 
            let (solved, rest) = Map.partition (fun k v -> Set.count v = 1) possible
            let toRemove = Map.toSeq solved |> Seq.map snd |> Set.unionMany
            let newPossible = Map.map (fun _ v -> Set.difference v toRemove) rest
            sift newPossible (Map.toList solved@mappings)

let solution = sift possible []

let answer = List.fold (fun acc i -> acc*int64 myTicket.[solution.[i]]) 1L [0..5]

printfn $"{answer}"