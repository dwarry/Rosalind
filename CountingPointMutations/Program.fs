// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open System.IO

[<EntryPoint>]
let main argv = 
    use f = File.OpenText "rosalind_hamm.txt"
    let s = f.ReadLine();
    let t = f.ReadLine();
    printfn "%d" (Rosalind.hammingDistance s t)



    0 // return an integer exit code
