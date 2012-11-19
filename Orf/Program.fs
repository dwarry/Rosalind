// http://rosalind.info/problems/prot
open System.IO

open Dna
open Rna
open AminoAcids

[<EntryPoint>]
let main argv = 
    
    use f = File.OpenText("rosalind_orf.txt")

    let line = f.ReadLine()

    let dna = dnaStringToDnaBaseSeq line
    
    let dna' = reverseComplement dna

    let proteinStrings = 
        let all = Seq.append (findAllPossibleProteins dna)
                             (findAllPossibleProteins dna') 
            
        let seqs = seq {
            for aaList in all do
                yield List.toSeq aaList |> proteinSeqToString
        } 

        seqs |> Seq.distinct
        
    for prots in proteinStrings do printfn "%s" prots
    
    0