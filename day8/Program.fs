open System.IO

type Inst = Jmp of int | Acc of int | Nop of int

type Machine = {
    program : Inst[];
    pc : int;
    acc : int
}

let execute machine =
    match machine.program.[machine.pc] with
        | Jmp offset -> { machine with pc = machine.pc + offset }
        | Acc arg -> { machine with pc = machine.pc + 1; acc = machine.acc + arg }
        | Nop arg -> { machine with pc = machine.pc + 1 }

let parseInst (str : string) =
    let parts = str.Split(" ")
    let arg = int parts.[1]
    match parts.[0] with
        | "jmp" -> Jmp arg
        | "acc" -> Acc arg
        | "nop" -> Nop arg
        | _ -> failwith "unknown instruction"

let runUntilRepeat machine =
    let mutable visited = Set.empty
    let mutable state = machine
    while state.pc < Array.length state.program && not (Set.contains state.pc visited) do
        visited <- Set.add state.pc visited
        state <- execute state
    (state.acc, state.pc >= Array.length state.program)

let program = File.ReadAllLines "../data/input8.txt" |> Array.map parseInst

let init = { program = program; pc = 0; acc = 0 }

let res = runUntilRepeat init

printfn $"{fst res}"

let mutate machine =
    let np = Array.copy machine.program
    let mutated = match np.[machine.pc] with
                    | Jmp off -> Array.set np machine.pc (Nop off); true
                    | Nop off -> Array.set np machine.pc (Jmp off); true
                    | _ -> false
    (mutated, { machine with program = np })

let solve2 machine =
    let mutable state = machine
    let mutable notFound = true
    while notFound do
        let (mutated, mutant) = mutate state
        if (mutated) then
            let (fstate, finished) = runUntilRepeat mutant
            notFound <- not finished
            if finished then printfn $"{fstate}"
        state <- execute state

solve2 init