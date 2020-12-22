open Utils.IO

let data = ReadGroupedText "../data/input22.txt"

let start = Array.map (fun (d:string[]) -> List.ofArray d.[1..] |> List.map int) data

let scoreDeck deck = List.rev deck |> List.mapi (fun i v -> (i+1)*v) |> List.sum
let score players =
    let player = Array.findIndex (fun l -> not (List.isEmpty l)) players
    (player, scoreDeck players.[player])

let rec play players =
    if Array.exists List.isEmpty players 
        then score players
        else
            let topCards = Array.map List.head players |> Array.toList
            let next = Array.map List.tail players
            let winner = if topCards.[0] > topCards.[1] then 0 else 1
            next.[winner] <- next.[winner]@(List.sortDescending topCards)
            play next

printfn $"{play start}"

let play2 players =
    let rec game config players =
        if Array.exists List.isEmpty players || Set.contains players config
            then if Set.contains players config
                    then (0, scoreDeck players.[0]) else score players
            else
                let topCards = Array.map List.head players
                let next = Array.map List.tail players
                let playSubgame = Seq.forall2 (fun t n -> t <= List.length n) topCards next
                let winner = if playSubgame 
                                then
                                    let decks = Array.map2 List.take topCards next 
                                    game Set.empty decks |> fst
                                else if topCards.[0] > topCards.[1] then 0 else 1
                next.[winner] <- next.[winner]@([topCards.[winner]; topCards.[(winner+1)%2]])
                game (Set.add players config) next
    game Set.empty players

printfn $"{play2 start}"