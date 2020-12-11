open System.IO

let init = File.ReadAllLines "../data/input11.txt"
let height = Array.length init
let width = String.length init.[0]

let dirs = [(-1,-1); (0, -1); (1,-1); (-1, 0); (1, 0); (-1,1); (0,1); (1,1)]

let inbounds dx dy = dx >= 0 && dx < width && dy >= 0 && dy < height
let neighbors x y = seq { for (dx, dy) in dirs do if inbounds (x+dx) (y+dy) then (x+dx,y+dy) }

let nextState (data:string[]) x y =
    let count x y = Seq.filter (fun (x,y) -> data.[y].[x] = '#') (neighbors x y) |> Seq.length
    match data.[y].[x] with
        | '#' when count x y >= 4 -> 'L'
        | 'L' when count x y = 0 -> '#'
        | x -> x

let update data updater = Array.mapi (fun y s -> String.mapi (fun x c -> updater data x y) s) data

let compare data1 data2 = Array.forall2 (=) data1 data2

let census data = Array.map (fun s -> String.filter ((=) '#') s |> String.length) data
                     |> Array.sum

let rec runUntilStatic data updater =
    let next = update data updater
    if compare next data
        then census next
        else runUntilStatic next updater

let res1 = runUntilStatic init nextState
printfn $"{res1}"

let rec scan (data:string[]) (dx, dy) x y =
    let (nx, ny) = (x+dx, y+dy)
    inbounds nx ny && match data.[ny].[nx] with
                        | '.' -> scan data (dx, dy) nx ny
                        | '#' -> true
                        | _ -> false

let nextState2 (data:string[]) x y =
    let count x y = Seq.filter (fun dir -> scan data dir x y) dirs |> Seq.length
    match data.[y].[x] with
        | '#' when count x y >= 5 -> 'L'
        | 'L' when count x y = 0 -> '#'
        | x -> x

let res2 = runUntilStatic init nextState2

printfn $"{res2}"