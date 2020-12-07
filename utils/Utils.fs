namespace Utils

open System
open System.IO

module IO =
    let ReadAllLines file =
        use f = File.OpenText file
        let content = f.ReadToEnd()
        content.Split('\n', StringSplitOptions.RemoveEmptyEntries)
    let split (on : string) (str : string) = str.Split(on, StringSplitOptions.RemoveEmptyEntries)
    let ReadGroupedText file =
        let split (on : string) (str : string) = str.Split(on, StringSplitOptions.RemoveEmptyEntries)
        File.ReadAllText file |> split "\n\n" |> Array.map (split "\n")