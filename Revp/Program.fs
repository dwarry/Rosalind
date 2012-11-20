// http://rosalind.info/problems/prot
open System.IO

open Dna

[<EntryPoint>]
let main argv = 
    let isReversePalindrome (dna: seq<DnaBase>) = (Seq.forall2 (fun x y -> x = y) dna (reverseComplement dna))

    let palindromes (n: int) (dna: seq<DnaBase>) = seq {
            
        let windows = Seq.windowed n dna

        let indices = seq {1 .. System.Int32.MaxValue }

        for (w, i) in Seq.zip windows indices do
            if  Array.toSeq w |> isReversePalindrome  then
                yield (i, n)
    }
        
    use f = File.OpenText("rosalind_revp.txt")

    use output = File.CreateText("results.txt")

    let line = f.ReadLine()

    let dna = dnaStringToDnaBaseSeq line
    
    let lengths = seq { 4 .. 8 }

    let results = Seq.map (fun x -> palindromes x dna) lengths |> Seq.concat

    for (pos, len) in results do
        output.WriteLine("{0} {1}", pos, len)
        
    
    0