// http://rosalind.info/problems/prot
open System.IO
open Dna

[<EntryPoint>]
let main argv = 
    use f = File.OpenText "rosalind_subs.txt"
    let dnaSeq = f.ReadLine() |> dnaStringToDnaBaseSeq
    let subSeq = f.ReadLine() |> dnaStringToDnaBaseSeq
    let slices = Seq.windowed (Seq.length subSeq) dnaSeq
    
    let results = Seq.fold (fun st slice -> let sq = Seq.ofArray slice
                                            let index = fst st
                                            let matches = snd st
                                            if  Seq.forall2 (fun a b -> (a = b)) sq subSeq then
                                                (1 + index, index :: matches)
                                            else
                                                (1 + index, matches)) 
                           (1, [])
                           slices
    

    use output = File.CreateText "result.txt"
    for index in snd results |> List.rev do 
        output.Write(index)
        output.Write(" ")

    0 // return an integer exit code
