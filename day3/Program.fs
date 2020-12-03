open Utils.IO

let collision (tm : string[]) (x, y) = 
    let row = tm.[y]
    row.[x % String.length row] = '#'

let makeTrajectory maxh (dx, dy) =
    List.unfold (fun (x, y) ->
                    if y < maxh
                        then Some ((x, y), (x+dx, y+dy))
                        else None)
                (0, 0)

let data = ReadAllLines "../data/input3.txt"
let trajectory = makeTrajectory (Array.length data) (3, 1)
let collisions t = Seq.filter (collision data) t |> Seq.length |> int64

printfn $"{collisions trajectory}"

let trajectories = List.map (makeTrajectory (Array.length data))
                          [(1,1); (3,1); (5,1); (7,1); (1,2)]

let result = List.fold (fun s t -> s * collisions t) 1L trajectories

printfn $"{result}"