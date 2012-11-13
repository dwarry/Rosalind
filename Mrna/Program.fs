// http://rosalind.info/problems/prot
open System.IO
open AminoAcids
open Rna


[<EntryPoint>]
let main argv = 
    use f = File.OpenText "rosalind_mrna.txt"
    let line = f.ReadLine()
    let aminoAcids = Seq.map charToAminoAcid line
    let codonPossibilities = 
        Seq.map Some aminoAcids |>
        Seq.map aminoAcidToCodons 

    let result = Seq.fold 
                     (fun acc cs ->
                          let len = Seq.length cs 
                          ( acc * len ) % 1000000)
                          
                     3
                     codonPossibilities
    printfn "%d" result
    
    0 // return an integer exit code
