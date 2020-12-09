open System.IO

let data = File.ReadAllLines "../data/input9.txt" |> Array.map int64 
let windows = Seq.windowed 26 data

let lacksSum (win : int64[]) =
    let nums = win.[0..24] |> Set.ofSeq
    Set.exists (fun n -> Set.contains (win.[25] - n) nums) nums |> not

let target = Seq.find lacksSum windows |> fun w -> w.[25] 

printfn $"{target}"

let findRange target =
    let mutable lo = 0
    let mutable hi = 1
    let mutable found = false
    while not found do
        let s = Seq.sum data.[lo..hi]
        if s = target 
            then found <- true
            else if s > target
                then lo <- lo + 1
                else hi <- hi + 1
    Seq.min data.[lo..hi] + Seq.max data.[lo..hi]

let res = findRange target
printfn $"{res}"