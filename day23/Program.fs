open System.Collections.Generic

let start = [ 5; 9; 8; 1; 6; 2; 7; 3; 4 ]

let test = [ 3; 8; 9; 1; 2; 5; 4; 6; 7 ]

let step cups size =
    let lower n = if n = 1 then size else n-1
    let (x::xs, ys) = List.splitAt 4 cups
    let rec findDest n = if List.contains n xs
                              then findDest (lower n)
                              else n
    let dest = findDest (lower x)
    let destIndex = List.findIndexBack ((=) dest) ys
    let (prefix, suffix) = List.splitAt (destIndex+1) ys
    List.concat (seq { prefix; xs; suffix; [x]})

let run iter init = 
    let rec worker n xs size = if n = 0 then xs else worker (n-1) (step xs size) size
    worker iter init (List.length init)

let t = run 100 start

for i in t do printf $"{i}"
printfn ""

let test2 = List.append test [10..1000000]
let start2 = List.append start [10..1000000]

let solve2 list =
    let nodeMap = Dictionary<int, LinkedListNode<int>> ()
    let cups = LinkedList<int> ()
    List.iter (fun x -> nodeMap.[x] <- cups.AddLast x) list
    let size = List.length list
    let lower n = if n = 1 then size else n-1
    let mutable currentNode = cups.First
    let getNext node = if node = cups.Last then cups.First else node.Next
    let slurp3 pos =
        let a = getNext pos
        let b = getNext a
        let c = getNext b
        let removed = [a; b; c]
        List.iter (fun (x:LinkedListNode<int>) -> cups.Remove x) removed
        removed
    let add3 pos nodes =
        List.fold (fun s (v:LinkedListNode<int>) -> cups.AddAfter (s, v); getNext s) pos nodes
    let rec findDest n removed =
        if List.exists (fun (v:LinkedListNode<int>) -> v.Value = n) removed 
            then findDest (lower n) removed else n
    for i in 0..10000000 do
        let removed = slurp3 currentNode
        let dest = findDest (lower currentNode.Value) removed
        let _ = add3 nodeMap.[dest] removed
        currentNode <- getNext currentNode
    let one = nodeMap.[1]
    let a = getNext one
    let b = getNext a
    (int64 a.Value)*(int64 b.Value)

printfn $"{solve2 start2}"