open System.IO

let data = File.ReadAllLines "../data/input9.txt" |> Array.map int64 
let windows = Seq.windowed 26 data

let lacksSum (win : int64[]) =
    let nums = win.[0..24] |> Set.ofSeq
    Set.exists (fun n -> Set.contains (win.[25] - n) nums) nums |> not

let target = Seq.find lacksSum windows |> fun w -> w.[25] 

printfn $"{target}"

let findRange target =
    let rec worker lo hi sum =
        if sum = target
            then Seq.min data.[lo..hi] + Seq.max data.[lo..hi]
            else if sum > target
                then worker (lo + 1) hi (sum - data.[lo])
                else worker lo (hi + 1) (sum + data.[hi + 1])
    worker 0 0 data.[0]

let res = findRange target
printfn $"{res}"