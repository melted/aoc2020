open System.Text.RegularExpressions
open Utils.IO

type Tile = {
    Nbr : int
    Data : bool [,]
}

let data = ReadGroupedText "../data/input20.txt"

let titleRegex = Regex @"^Tile (\d+):$"

let parseTile (lines:string[]) =
    let tileNbr = (titleRegex.Match lines.[0]).Groups.[1].Value |> int
    let srcMap = lines.[1..]
    let initFn x y = srcMap.[y].[x] = '#'
    { Nbr = tileNbr; Data = Array2D.init (Array.length srcMap) (String.length srcMap.[0]) initFn }

let edgeId b =
    let num b = Array.mapi (fun i v -> if v then pown 2 i else 0) b |> Array.sum
    min (num b) (num (Array.rev b))

let getEdge (arr:bool[,]) = function
    | 0 -> arr.[*, 0]
    | 1 -> arr.[Array2D.length1 arr - 1 , *]
    | 2 -> arr.[*, Array2D.length2 arr - 1]
    | 3 -> arr.[0,*]

let edges tile =
    Array.map (fun i -> edgeId (getEdge tile.Data i)) [| 0..3|]

let tiles = Array.map parseTile data |> List.ofArray |> List.sort

let tileMap = Seq.map (fun t -> (t.Nbr, t)) tiles |> Map.ofSeq

let mutable edgeMap = Map.empty

for t in tiles do
    for e in edges t do
        edgeMap <- Map.change e (fun v -> Option.map (fun xs -> t.Nbr::xs) v |> Option.orElse (Some [t.Nbr])) edgeMap

let unmatched = Map.toSeq edgeMap |> Seq.filter (fun (_, v) -> List.length v = 1) |> Seq.map (snd >> List.head) |> Seq.sort

let corners = Seq.windowed 2 unmatched |> Seq.filter (fun a -> a.[0] = a.[1]) |> Seq.map (fun a -> int64 a.[0])

let res = Seq.fold (*) 1L corners 

printfn $"{res}"

let image = Array2D.create 96 96 false

let placement = Array2D.create 12 12 None

placement.[0,0] <- Some tileMap.[1439]

let rotate arr =
    let ny = Array2D.length2 arr - 1
    Array2D.mapi (fun x y v -> arr.[y, ny-x]) arr

let flip arr =
    let ny = Array2D.length2 arr - 1
    Array2D.mapi (fun x y v -> arr.[x, ny-y]) arr

let hflip arr =
    let nx = Array2D.length1 arr - 1
    Array2D.mapi (fun x y v -> arr.[nx-x, y]) arr

let rec adjust arr edgeNbr edge =
    let rightBits a b = a = b || Array.rev a = b
    let e = getEdge arr edgeNbr
    if rightBits e edge
        then if e = edge 
                then arr
                else if edgeNbr = 0 || edgeNbr = 2
                        then adjust (hflip arr) edgeNbr edge
                        else adjust (flip arr) edgeNbr edge
        else adjust (rotate arr) edgeNbr edge

let printImage (im:bool[,]) =
    for y in 0..Array2D.length2 im - 1 do
        for x in 0..Array2D.length1 im - 1 do
            let c = if im.[x,y] then '#' else '.'
            printf $"{c}"
        printfn ""

for y in 0..11 do
    for x in 0..11 do
        if Option.isNone placement.[x,y] then
            let (tile, edge, edgeNbr) =
                if x = 0 
                    then 
                        let tile = Option.get placement.[x, y-1]
                        (tile, getEdge tile.Data 2, 0)
                    else
                        let tile = Option.get placement.[x-1, y]
                        (tile, getEdge tile.Data 1, 3)
            let newTile = edgeMap.[edgeId edge] |> List.filter ((<>) tile.Nbr) |> List.head
            let newData = adjust tileMap.[newTile].Data edgeNbr edge
            placement.[x,y] <- Some { Nbr = newTile; Data = newData }

for y in 0..11 do
    for x in 0..11 do
        let tile = Option.get placement.[x,y]
        Array2D.blit tile.Data 1 1 image (x*8) (y*8) 8 8

let flipped = flip image

let mutable count = 0

Array2D.iter (fun v -> if v then count <- count+1) image

let monster = [ (18,0); (0,1); (5,1); (6,1); (11,1); (12,1); (17,1); (18,1); (19,1);
                (1,2); (4,2); (7,2); (10,2); (13,2); (16,2)]

let mutable hits = 0;

for y in 0..92 do
    for x in 0..76 do
        let hit = List.forall (fun (dx, dy)-> flipped.[x+dx, y+dy]) monster
        if hit then hits <- hits+1

printfn $"{count - hits*15}"
