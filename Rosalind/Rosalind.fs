module Rosalind

open System.IO
open System.Text.RegularExpressions

type public DnaBase = |A|C|G|T

type public RnaBase = |A|C|G|U

type public DnaBaseCount = {A:int; C:int; G:int; T:int}

type private FastaRecord = {id:string; bases:seq<DnaBase>}

let public charToDnaBase = 
    function | 'A' | 'a' -> DnaBase.A
             | 'C' | 'c' -> DnaBase.C
             | 'G' | 'g' -> DnaBase.G
             | 'T' | 't' -> DnaBase.T
             | _ -> failwith "Unknown value"

let public dnaBaseToChar = 
    function | DnaBase.A -> 'A'
             | DnaBase.C -> 'C'
             | DnaBase.G -> 'G'
             | DnaBase.T -> 'T'


let public charToRnaBase = 
    function | 'A' | 'a' -> RnaBase.A
             | 'C' | 'c' -> RnaBase.C
             | 'G' | 'g' -> RnaBase.G
             | 'U' | 'u' -> RnaBase.U
             | _ -> failwith "Unknown value"

let public rnaBaseToChar =
    function | RnaBase.A -> 'A'
             | RnaBase.C -> 'C'
             | RnaBase.G -> 'G'
             | RnaBase.U -> 'U'    

let public dnaToRna = 
    function | DnaBase.A -> RnaBase.A
             | DnaBase.C -> RnaBase.C
             | DnaBase.G -> RnaBase.G
             | DnaBase.T -> RnaBase.U

let public rnaToDna = 
    function | RnaBase.A -> DnaBase.A 
             | RnaBase.C -> DnaBase.C
             | RnaBase.G -> DnaBase.G
             | RnaBase.U -> DnaBase.T

let public dnaComplement = 
    function | DnaBase.A -> DnaBase.T
             | DnaBase.C -> DnaBase.G
             | DnaBase.G -> DnaBase.C
             | DnaBase.T -> DnaBase.A



let public dnaStringToDnaBaseSeq (s: string) = Seq.map charToDnaBase s

let public dnaBaseSeqToString dna = 
    let chars = Seq.map dnaBaseToChar dna
    new string(Array.ofSeq chars)

let public rnaBaseSeqToString rna = 
    let chars = Seq.map rnaBaseToChar rna
    new string(Array.ofSeq chars)

let public countBases bases = 
    Seq.fold (fun bc b -> match b with
                          | DnaBase.A -> {bc with A=bc.A + 1}
                          | DnaBase.C -> {bc with C=bc.C + 1}
                          | DnaBase.G -> {bc with G=bc.G + 1}
                          | DnaBase.T -> {bc with T=bc.T + 1})
              {A=0;C=0;G=0;T=0} 
              bases



let public gcContent bases =
   let counts = countBases bases
   100.0 * float(counts.C + counts.G) / float(counts.A + counts.C + counts.G + counts.T)

let public dnaStringToRnaString (s: string) = dnaStringToDnaBaseSeq s |> Seq.map dnaToRna |> rnaBaseSeqToString;


// Problem 3
let reverseComplement (s: string) = 
    let bases = dnaStringToDnaBaseSeq s |> Seq.map dnaComplement |> Array.ofSeq |> Array.rev
    dnaBaseSeqToString bases
    

// Converts a sequence of lines in the FASTA format described in 
// http://rosalind.info/problems/gc/ to a sequence of 
let public fasta (lines: seq<string>) =
   seq { 
      let id = ref ""
      let sb = new System.Text.StringBuilder(1024)
 
      for line in (Seq.append lines [">"]) do
          if line.StartsWith(">") then
              if !id <> "" then
                  yield (!id, sb.ToString() |> dnaStringToDnaBaseSeq )
              id := line.Substring(1)
              sb.Clear() |> ignore
          else
              sb.Append(line) |> ignore
   }

// Problem "GC Content"
let public fastaToGcContent (lines: seq<string>) =
    let results = Seq.map (function | (id, bases)-> (id, gcContent bases))  (fasta lines)
    Seq.maxBy (fun (x: (string * float)) -> snd x) results
   
    

//[<EntryPoint>]
let main argv = 
    //let p1 = Problem1.countBases "rosalind_dna.txt"
    //printfn "%d %d %d %d" p1.A p1.C p1.G p1.T
    
    //let p2 = Problem2.dnaToRna "rosalind_rna.txt"
    //printfn "%s" p2
    
    //use f = File.OpenText "rosalind_revc.txt"
    //use result = File.CreateText("result.txt")
    //let data = f.ReadToEnd() 
    //let p3 = reverseComplement data
    //result.WriteLine(p3)    
    
    0 // return an integer exit code
