// http://rosalind.info/problems/prot
open System.IO

open Dna


[<EntryPoint>]
let main argv = 
    let lines = 
        seq [
            use f = File.OpenText "rosalind_cons.txt"
            let line = ref (f.ReadLine())
           
            while !line <> null do
                yield !line
                line := f.ReadLine()
        ]
 
    let dnaSeqs = Seq.map dnaStringToDnaBaseSeq lines
    
    let profile = makeProfile dnaSeqs

    let mostLikelyDnaString = Seq.map mostFrequent profile
    printfn "%s" 

    let sbA = new System.Text.StringBuilder("A: ")
    let sbC = new System.Text.StringBuilder("C: ")
    let sbG = new System.Text.StringBuilder("G: ")
    let sbT = new System.Text.StringBuilder("T: ")

    Seq.iter (fun bc -> sbA.AppendFormat("{0} ", bc.A) |> ignore
                        sbC.AppendFormat("{0} ", bc.C) |> ignore
                        sbG.AppendFormat("{0} ", bc.G) |> ignore
                        sbT.AppendFormat("{0} ", bc.T) |> ignore) profile

    use output = File.CreateText ("result.txt")
    output.WriteLine(dnaBaseSeqToString mostLikelyDnaString)
    output.WriteLine(sbA);
    output.WriteLine(sbC);
    output.WriteLine(sbG);
    output.WriteLine(sbT);
    
    0 // return an integer exit code
