open System.IO
open System.Runtime.Intrinsics.X86
open System.Text.RegularExpressions

type Instruction = Mask of uint64*uint64 | Mem of uint64*uint64

let memRegex = Regex @"^mem\[(\d+)\] = (\d+)$"
let maskRegex = Regex @"^mask = ((1|0|X)+)$"

let parseInstruction str =
    if memRegex.IsMatch str 
        then
            let m = memRegex.Match str
            Mem (uint64 m.Groups.[1].Value, uint64 m.Groups.[2].Value)
        else if maskRegex.IsMatch str
                then 
                    let m = maskRegex.Match str
                    let handle (s, m, i) ch =
                        match ch with
                            | '1' -> (s+(1UL<<<i), m, i-1)
                            | 'X' -> (s, m+(1UL<<<i), i-1)
                            | _ -> (s, m, i-1)
                    let (fix, mask, _) = Seq.fold handle (0UL, 0UL, 35) (m.Groups.[1].Value)
                    Mask (fix, mask)
                else failwith "Unknown instruction"

let prog = File.ReadAllLines "../data/input14.txt" |> Array.map parseInstruction |> Array.toList

let run prog =
    let rec worker memory  (fix, mask) = function
        | [] -> memory |> Map.toSeq |> Seq.map snd |> Seq.sum
        | inst::rest ->
            match inst with
                | Mem (addr, value) -> worker (Map.add addr (fix+(mask&&&value)) memory)
                                              (fix, mask) rest
                | Mask (f, m) -> worker memory (f, m) rest
    worker Map.empty (0UL, 0UL) prog

printfn $"{run prog}"

let firstBit i =
    let zeros = Lzcnt.X64.LeadingZeroCount i
    1UL<<<(63 - int zeros)

let rec generateMasks = function
    | 0UL -> [0UL]
    | mask -> let bit = firstBit mask
              let rest = generateMasks (mask &&& ~~~bit)
              let more = List.map (fun m -> m ||| bit) rest
              rest@more

let run2 prog =
    let rec worker memory (fix, mask) = function
        | [] -> memory |> Map.toSeq |> Seq.map snd |> Seq.sum
        | inst::rest ->
            match inst with
                | Mask (f, m) -> worker memory (f, m) rest
                | Mem (addr, value) ->
                    let addFix = (addr ||| fix) &&& ~~~mask
                    let writes = List.map (fun m -> addFix ||| m) (generateMasks mask)
                    let newMem = List.fold (fun mem a -> Map.add a value mem)
                                                   memory writes
                    worker newMem (fix, mask) rest
    worker Map.empty (0UL, 0UL) prog

printfn $"{run2 prog}"