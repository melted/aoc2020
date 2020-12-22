open System.IO
open Utils.IO

let parse str =
    let parts = split "(contains" str
    let ingredients = split " " parts.[0] |> Set.ofArray
    let allergens = split "," parts.[1] |> Array.map (fun s -> s.Trim ([| ')'; ','; ' ' |])) |> Set.ofArray
    (ingredients, allergens)

let data = File.ReadAllLines "../data/input21.txt" |> Array.map parse

let all = Array.map fst data |> Set.unionMany

let allergens = Array.map snd data |> Set.unionMany |> Set.toSeq |> Seq.sort

let allergenFree a =
    let candidates = Array.filter (fun (_,s) -> Set.contains a s) data |> Array.map fst |> Set.intersectMany
    Set.difference all candidates

let findAllergen a =
    let candidates = Array.filter (fun (_,s) -> Set.contains a s) data |> Array.map fst |> Set.intersectMany
    candidates

let allFree = Seq.map allergenFree allergens |> Set.intersectMany

let count = Array.sumBy (fun (i, _) -> Set.filter (fun s -> Set.contains s allFree) i |> Set.count) data

printfn $"{count}"

let mapping =
    let candidates = Seq.map (fun a -> (a, findAllergen a)) allergens |> Map.ofSeq
    let rec sift found cands =
        if Map.isEmpty cands
            then found
            else
                let (solved, rest) = Map.partition (fun a c -> Set.count c = 1) cands
                let toRemove = Map.toSeq solved |> Seq.map snd |> Set.unionMany
                let newCand = Map.map (fun a c -> Set.difference c toRemove) rest
                sift (Map.toList solved@found) newCand
    sift [] candidates |> List.sort

for (_,b) in mapping do printf $"{Set.minElement b},"
 