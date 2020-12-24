open System.IO

type dir = E | W | NW | NE | SW | SE

let parse str =
    let rec worker acc = function
        | [] -> List.rev acc
        | 's'::'e'::xs -> worker (SE::acc) xs
        | 's'::'w'::xs -> worker (SW::acc) xs
        | 'n'::'e'::xs -> worker (NE::acc) xs
        | 'n'::'w'::xs -> worker (NW::acc) xs
        | 'w'::xs -> worker (W::acc) xs
        | 'e'::xs -> worker (E::acc) xs
    worker [] (List.ofSeq str)

let data = File.ReadAllLines "../data/input24.txt" |> Array.map parse

let move (x, y) = function
    | E -> (x+1, y)
    | W -> (x-1, y)
    | SW -> (x-1, y+1)
    | SE -> (x, y+1)
    | NW -> (x, y-1)
    | NE -> (x+1, y-1)

let positions = Array.map (fun ds -> List.fold move (0,0) ds) data

let black = Array.fold (fun s p -> if Set.contains p s then Set.remove p s else Set.add p s) 
                        Set.empty positions

printfn $"{Set.count black}"

let neighbors (x, y) = [ (x+1, y); (x-1, y); (x-1, y+1); (x, y+1); (x, y-1); (x+1, y-1)]

let evolve black =
    let census = 
        let update v = match v with
                        | None -> Some 1
                        | Some x -> Some (x+1)
        let addp c p = Map.change p update c
        Seq.fold addp Map.empty (Seq.collect neighbors black)
    let alive p c = c = 2 || (c = 1 && Set.contains p black) 
    Map.filter alive census |> Map.toSeq |> Seq.map fst |> Set.ofSeq

let mutable current = black

for i in 1..100 do current <- evolve current

printfn $"{Set.count current}"