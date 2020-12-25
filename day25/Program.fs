
let card = 3248366L
let door = 4738476L

let step start subj = (start * subj)%20201227L

let findLoop pub =
    let rec worker acc n = if acc = pub then n else worker (step acc 7L) (n+1)
    worker 1L 0

let cardLoop = findLoop card
let doorLoop = findLoop door

let mutable encKey1 = 1L
let mutable encKey2 = 1L
for i in 1..cardLoop do encKey1 <- step encKey1 door
for i in 1..doorLoop do encKey2 <- step encKey2 card
printfn $"{encKey1} {encKey2}"
