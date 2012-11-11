// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open System.IO

open Rosalind


[<EntryPoint>]
let main argv = 
    
(*
    let data = ">Rosalind_6404
CCTGCGGAAGATCGGCACTAGAATAGCCAGAACCGTTTCTCTGAGGCTTCCGGCCTTCCC
TCCCACTAATAATTCTGAGG
>Rosalind_5959
CCATCGGTAGCGCATCCTTAGTCCAATTAAGTCCCTATCCAGGCGCTCCGCCGAAGGTCT
ATATCCATTTGTCAGCAGACACGC
>Rosalind_0808
CCACCCTCGTGGTATGGCTAGGCATTCAGGAACCGGAGAACGCTTCAGACCAGCCCGGAC
TGGGAACCTGCGGGCAGTAGGTGGAAT".Split([|System.Environment.NewLine|], System.StringSplitOptions.RemoveEmptyEntries)
*)
    use f = File.OpenText "rosalind_gc.txt"
    let lines = seq { 
            let line = ref (f.ReadLine())
            while !line <> null do
                yield !line
                line := f.ReadLine()
        }
    let result = fastaToGcContent lines
    printfn "%s\n%2.4f" (fst result) (snd result)
    
    0 // return an integer exit code
