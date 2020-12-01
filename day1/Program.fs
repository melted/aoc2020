open System
open Utils.IO

let find target nums =
    Set.filter (fun n -> Set.contains (target - n) nums) nums |> Set.toList

let solve1 nums =
    let [a;b] = find 2020 nums
    a*b

let solve2 nums =
    let handle n = 
        match find (2020-n) nums with
          | [a; b] -> Some (n, a, b)
          | _ -> None
    let (a,b,c)::_ = List.choose handle (Set.toList nums)
    a*b*c

let data = ReadAllLines "../data/input1.txt" |> Array.map int |> Set.ofArray
let res1 = solve1 data
printfn $"{res1}"
let res2 = solve2 data
printfn $"{res2}"