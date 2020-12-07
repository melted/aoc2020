open System.IO
open System.Text.RegularExpressions

let regex = Regex(@"^(\w+ \w+) bags contain(( (\d+) (\w+ \w+) bags?.)+| no other bags.)$")
let parseData dict str = 
    let m = regex.Match(str)
    let bag = m.Groups.[1].Value
    let nums = Seq.map (fun (c:Capture) -> c.Value |> int) m.Groups.[4].Captures
    let bags = Seq.map (fun (c:Capture) -> c.Value) m.Groups.[5].Captures
    Map.add bag (Seq.zip nums bags) dict

let data = File.ReadAllLines "../data/input7.txt" |> Array.fold parseData Map.empty

let target = "shiny gold"

let rec findGoldBag state s =
    match Map.tryFind s state with
        | Some b -> state
        | None -> if s = target 
                    then Map.add s true state
                    else 
                        let children = Seq.map snd (Map.find s data)
                        if Seq.length children = 0
                            then Map.add s false state
                            else 
                                let newState = Seq.fold (fun st n -> enumerate st n) state children
                                Map.add s (Seq.exists (fun ch -> Map.find ch newState) children) newState

let res = Map.toSeq data |> Seq.map fst |> Seq.fold enumerate Map.empty 
            |> Map.filter (fun k v -> v) |> Map.count

printfn $"{res-1}"

let rec count t =
    let children = Map.find t data
    let childBags = Seq.fold (fun acc (i, n) -> acc+i*count n) 0 children
    childBags + 1

printfn $"{(count target)-1}"