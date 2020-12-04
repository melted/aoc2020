open System.IO
open System.Text.RegularExpressions

let readData path =
    let text = File.ReadAllLines path
    let handle (old, current) str =
        if str = ""
            then (current::old, Map.empty)
            else
                let pairs = str.Split(' ')
                let addOne mapping (pair : string) =
                        let kv : string array = pair.Split(':')
                        Map.add kv.[0] kv.[1] mapping
                let updated = Array.fold addOne current pairs
                (old, updated)
    Array.fold handle ([], Map.empty) text |> fun (xs, x) -> List.rev (x::xs)

let mandatory = ["byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid"]

let validate pp = List.forall (fun field -> Map.containsKey field pp) mandatory

let validateYear v min max =
    Regex(@"^\d\d\d\d$").Match(v).Success && (int v) >= min && (int v) <= max

let validateHeight v =
    let x = String.length v - 2
    let h = v.[..x-1]
    let suffix = v.[x..]
    match suffix with
        | "cm" -> (int h) >= 150 && (int h) <= 193
        | "in" -> (int h) >= 59 && (int h) <= 76
        | _ -> false

let validField key v =
    match key with
        | "byr" -> validateYear v 1920 2002
        | "iyr" -> validateYear v 2010 2020
        | "eyr" -> validateYear v 2020 2030
        | "hgt" -> validateHeight v
        | "hcl" -> Regex(@"^#[0-9a-z]{6}$").Match(v).Success
        | "ecl" -> List.contains v ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"]
        | "pid" -> Regex(@"^\d{9}$").Match(v).Success

let validate2 pp =
    List.forall (fun field -> Map.containsKey field pp && 
                                validField field pp.[field])
                mandatory

let data = readData "../data/input4.txt"
let validCount = List.filter validate data |> List.length
printfn $"{validCount}"

let validCount2 = List.filter validate2 data |> List.length
printfn $"{validCount2}"
