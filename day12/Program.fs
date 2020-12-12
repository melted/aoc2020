open System.IO

let data = File.ReadAllLines "../data/input12.txt" |> Array.map (fun s-> (s.[0], int s.[1..]))

let headings = [| (1,0); (0,1); (-1,0); (0,-1) |]
let navigate ((x, y), h) inst =
    match inst with
       | ('N', w) -> ((x, y-w), h)
       | ('S', w) -> ((x, y+w), h)
       | ('E', w) -> ((x+w, y), h)
       | ('W', w) -> ((x-w, y), h)
       | ('L', w) -> ((x, y), (h + 3*(w/90))%4)
       | ('R', w) -> ((x, y), (h + (w/90))%4)
       | ('F', w) -> let (dx, dy) = headings.[h]
                     ((x+w*dx, y+w*dy), h)
       | _ -> failwith "Unknown input"

let ((x,y),_) = Seq.fold navigate ((0,0),0) data

printfn $"{abs x + abs y}"

let navigate2 (((wx, wy), (x,y)) as state) inst =
    let rec rotate w ((wx, wy), p) =
        if w > 0 then rotate (w-90) ((-wy, wx), p) else ((wx, wy), p)
    match inst with
        | ('N', w) -> ((wx, wy-w), (x,y))
        | ('S', w) -> ((wx, wy+w), (x,y))
        | ('E', w) -> ((wx+w, wy), (x,y))
        | ('W', w) -> ((wx-w, wy), (x,y))
        | ('L', w) -> rotate (360-w) state
        | ('R', w) -> rotate w state
        | ('F', w) -> ((wx, wy), (x+w*wx, y+w*wy))
        | _ -> failwith "Unknown input"

let (_, (x2, y2)) = Seq.fold navigate2 ((10,-1), (0,0)) data

printfn $"{abs x2 + abs y2}"