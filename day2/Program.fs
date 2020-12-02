open System
open System.Text.RegularExpressions
open Utils.IO

type Entry = {
    min : int;
    max : int;
    letter : char;
    password : string
}

let regex = Regex(@"^(\d+)-(\d+) (.): (.*)$")
let parseEntry s =
    let matches = regex.Match(s)
    if matches.Success
        then Some {
            min = matches.Groups.[1].Value |> int;
            max = matches.Groups.[2].Value |> int;
            letter = matches.Groups.[3].Value.[0];
            password = matches.Groups.[4].Value }
        else None

let validateEntry { min=min; max=max; letter=ch; password=pw } =
    let n = Seq.filter (fun c -> c = ch) pw |> Seq.length
    n >= min && n <= max

let validateEntry2 { min=min; max=max; letter=ch; password=pw } =
    pw.[min-1] <> pw.[max-1] && (pw.[min-1] = ch || pw.[max-1] = ch)

let data = ReadAllLines "../data/input2.txt"

let testdata = [|
        "1-3 a: abcde";
        "1-3 b: cdefg";
        "2-9 c: ccccccccc"
    |]

let entries = Array.choose parseEntry data
let res = Seq.filter validateEntry entries |> Seq.length
printfn $"{res}"

let res2 = Seq.filter validateEntry2 entries |> Seq.length
printfn $"{res2}"