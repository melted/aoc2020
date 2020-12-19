open Utils.IO

type Rule = 
      Lit of char
    | Ref of int
    | Pair of Rule * Rule
    | Or of Rule * Rule

let data = ReadGroupedText "../data/input19-2.txt"

let parseRule rules str =
    let words = split ":" str
    let ruleNbr = words.[0] |> int
    let parseOne (str:string) =
        if str.[0] = '"' then Lit str.[1] else Ref (int str)
    let parseSequence str =
        let words = split " " str |> Array.map parseOne
        Array.fold (fun rule w -> Pair (rule, w)) words.[0] (Array.tail words)
    let parseOr str =
        let sides = split "|" str |> Array.map parseSequence
        Array.fold (fun rule w -> Or (rule, w)) sides.[0] (Array.tail sides)
    Map.add ruleNbr (parseOr words.[1]) rules

let rules = Array.fold parseRule Map.empty data.[0]

let rec satisfy (str:string) = function
    | Lit ch -> if String.length str > 0 && str.[0] = ch then [str.Substring 1] else []
    | Pair (r1, r2) -> let first  = satisfy str r1
                       List.collect (fun s -> satisfy s r2) first
    | Or (r1, r2) -> satisfy str r1@satisfy str r2
    | Ref i -> satisfy str rules.[i]

let valid rule str = satisfy str rule |> List.filter (fun s -> String.length s = 0) |> List.isEmpty |> not
let count = Seq.filter (valid rules.[0]) data.[1] |> Seq.length

printfn $"{count}"
