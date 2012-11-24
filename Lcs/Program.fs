// http://rosalind.info/problems/lcs
open System.IO

open Dna

open Kmp

let shortestLineAndRest (lines: seq<string>) = 
    Seq.fold (fun (st: (string list * string option)) (x: string) -> match st with
                          | ([], None) -> ([], Some x)
                          | (rest, Some y) -> if x.Length < y.Length then
                                                  (y::rest, Some x)
                                              else
                                                  (x::rest, Some y))
             ([], None)
             lines

[<EntryPoint>]
let main argv = 
        
    use f = File.OpenText("rosalind_lcs.txt")

    use output = File.CreateText("results.txt")

    let lines = seq {
        let line = ref (f.ReadLine())

        while !line <> null do
            yield !line
            line := f.ReadLine()
    }
    
    //let lines = ["GATTACA"; "TAGACCA"; "ATACA"] |> List.toSeq

    let result = match shortestLineAndRest lines with 
                 | (_, None)                   -> Seq.empty<DnaBase>
                 | ([], Some x)                -> Dna.dnaStringToDnaBaseSeq x
                 | (otherLines, Some shortest) -> 
                     let others =  List.map (fun (x: string) -> Array.toSeq (x.ToCharArray() )) otherLines
                     Kmp.findLongestSubstringMatch shortest others |> Seq.map charToDnaBase             
                    
    output.WriteLine(Dna.dnaBaseSeqToString result)
    printfn "Done!"
    0