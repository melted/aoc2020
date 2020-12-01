namespace Utils

open System
open System.IO

module IO =
    let ReadAllLines file =
        use f = File.OpenText file
        let content = f.ReadToEnd()
        content.Split('\n', StringSplitOptions.RemoveEmptyEntries)
