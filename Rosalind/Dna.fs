﻿module Dna

type public DnaBase = |A|C|G|T

type public DnaBaseCount = {A:int; C:int; G:int; T:int}


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

let public dnaComplement = 
    function | DnaBase.A -> DnaBase.T
             | DnaBase.C -> DnaBase.G
             | DnaBase.G -> DnaBase.C
             | DnaBase.T -> DnaBase.A


let public dnaStringToDnaBaseSeq (s: string) = Seq.map charToDnaBase s

let public dnaBaseSeqToString dna = 
    let chars = Seq.map dnaBaseToChar dna
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

// Problem 3
let reverseComplement (dna: seq<DnaBase>) = 
    dna |> Seq.map dnaComplement |> Array.ofSeq |> Array.rev |> Array.toSeq
    

let public makeProfile (dnaBaseSeqs: seq<seq<DnaBase>>) = 
    let mat = dnaBaseSeqs |> Matrix.Generic.ofSeq 
    seq{
        for i in seq { 0 .. (mat.NumCols - 1)} do
            let col = mat.Column(i)
            yield countBases col
    }

let public mostFrequent (bc: DnaBaseCount) =
    if bc.A >= bc.C && bc.A >= bc.G && bc.A >= bc.T then DnaBase.A
    else if bc.C >= bc.G && bc.C >= bc.T then DnaBase.C
         else if bc.G >= bc.T then DnaBase.G
              else DnaBase.T