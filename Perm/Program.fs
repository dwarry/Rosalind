open Rosalind
open Permutations

// http://rosalind.info/problems/perm/
[<EntryPoint>]
let main argv = 
    let perms = Permutations.getPerms2 6 [1..6]
    printfn "%A" <| Seq.length perms

    for p in perms do printfn "%A" p

    0 // return an integer exit code
