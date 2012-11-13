// http://rosalind.info/problems/prot
open System.IO
open Rna
open AminoAcids

[<EntryPoint>]
let main argv = 
    use f = File.OpenText "rosalind_prtm.txt"
    let mass = f.ReadLine() |> proteinStringToSeq |> calculateMassOfProteinSeq
    
    use output = File.CreateText "result.txt"
    printfn "%0.2f" mass
    0 // return an integer exit code
