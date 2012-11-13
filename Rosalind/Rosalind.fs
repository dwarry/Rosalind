module Rosalind

open System.IO
open System.Text.RegularExpressions
open Dna
open Rna
open AminoAcids



type private FastaRecord = {id:string; bases:seq<DnaBase>}


// Converts a sequence of lines in the FASTA format described in 
// http://rosalind.info/problems/gc/ to a sequence of 
let public fasta (lines: seq<string>) =
   seq { 
      let id = ref ""
      let sb = new System.Text.StringBuilder(1024)
 
      for line in (Seq.append lines [">"]) do
          if line.StartsWith(">") then
              if !id <> "" then
                  yield (!id, sb.ToString() |> Dna.dnaStringToDnaBaseSeq )
              id := line.Substring(1)
              sb.Clear() |> ignore
          else
              sb.Append(line) |> ignore
   }

// Problem "GC Content"
let public fastaToGcContent (lines: seq<string>) =
    let results = Seq.map (function | (id, bases)-> (id, gcContent bases))  (fasta lines)
    Seq.maxBy (fun (x: (string * float)) -> snd x) results
   
// Problem "Counting Point Mutations"
let public hammingDistance (s: string) (t: string) = 
    Seq.zip s t |> Seq.filter (fun x -> (fst x) <> (snd x)) |> Seq.length

