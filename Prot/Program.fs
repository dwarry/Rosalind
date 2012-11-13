// http://rosalind.info/problems/prot
open System.IO
open Rna
open AminoAcids


[<EntryPoint>]
let main argv = 
    use f = File.OpenText "rosalind_prot.txt"
    let rna = f.ReadLine() |> rnaStringToRnaBaseSeq
    let protein = rnaBaseSeqToProteinSeq rna |> Seq.head |> proteinSeqToString
    use output = File.CreateText "result.txt"
    output.WriteLine(protein)
    0 // return an integer exit code
