open System
open System.IO

let eval str =
    let tokens = List.ofSeq str |> List.filter ((<>) ' ')
    let rec evalVal = function 
        | [] -> failwith "expected value"
        | '('::xs -> evalExpr xs
        | x::xs when Char.IsDigit x -> (int64 x - 48L, xs)
    and evalOps acc = function
        | [] -> (acc, [])
        | '*'::xs -> let (n, ys) = evalVal xs 
                     (acc*n, ys)
        | '+'::xs -> let (n, ys) = evalVal xs 
                     (acc+n, ys)
    and evalExpr toks =
        let (v,t) = evalVal toks
        let rec loop acc = function
            | [] -> (acc, [])
            | ')'::xs -> (acc, xs)
            | xs -> let (n, ys) = evalOps acc xs
                    loop n ys
        loop v t
    evalExpr tokens |> fst

let eval2 str =
    let tokens = List.ofSeq str |> List.filter ((<>) ' ')
    let rec evalVal = function 
        | [] -> failwith "expected value"
        | '('::xs -> evalExpr xs
        | x::xs when Char.IsDigit x -> (int64 x - 48L, xs)
    and evalTerm acc toks =
        let (v,t) = evalVal toks
        match t with
           | '+'::xs -> evalTerm (v+acc) xs
           | xs -> (v+acc, xs)
    and evalExpr toks =
        let (v,t) = evalVal toks
        let rec loop acc = function
            | [] -> (acc, [])
            | ')'::xs -> (acc, xs)
            | '*'::xs -> let (n, ys) = evalTerm 0L xs
                         loop (n*acc) ys
            | '+'::xs -> let (n,ys) = evalTerm acc xs
                         loop n ys
        loop v t
    evalExpr tokens |> fst

let data = File.ReadAllLines "../data/input18.txt"

let res = Array.sumBy (fun s -> eval s) data
let res2 = Array.sumBy (fun s -> eval2 s) data

printfn $"{res}"
printfn $"{res2}"