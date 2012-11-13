module Rna

open Dna
open AminoAcids

type public RnaBase = |A|C|G|U

type public Codon = (RnaBase * RnaBase * RnaBase)


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

let public rnaStringToRnaBaseSeq (s: string) = Seq.map charToRnaBase s


let public rnaBaseSeqToString rna = 
    let chars = Seq.map rnaBaseToChar rna
    new string(Array.ofSeq chars)

    
let public dnaStringToRnaString (s: string) = dnaStringToDnaBaseSeq s |> Seq.map dnaToRna |> rnaBaseSeqToString;


// Maps triples of RnaBases to the corresponding amino acid for which they code.
// If the value is None, the combination represents a Stop codon
let private initCodonTable = 
    let result = new System.Collections.Generic.Dictionary<Codon, AminoAcid option>()
    result.Add((RnaBase.U, RnaBase.U, RnaBase.U), Some AminoAcid.F)                  
    result.Add((RnaBase.U, RnaBase.U, RnaBase.C), Some AminoAcid.F)                  
    result.Add((RnaBase.U, RnaBase.U, RnaBase.A), Some AminoAcid.L)                  
    result.Add((RnaBase.U, RnaBase.U, RnaBase.G), Some AminoAcid.L)                  
    result.Add((RnaBase.U, RnaBase.C, RnaBase.U), Some AminoAcid.S)                  
    result.Add((RnaBase.U, RnaBase.C, RnaBase.C), Some AminoAcid.S)                  
    result.Add((RnaBase.U, RnaBase.C, RnaBase.A), Some AminoAcid.S)                  
    result.Add((RnaBase.U, RnaBase.C, RnaBase.G), Some AminoAcid.S)                  
    result.Add((RnaBase.U, RnaBase.A, RnaBase.U), Some AminoAcid.Y)                  
    result.Add((RnaBase.U, RnaBase.A, RnaBase.C), Some AminoAcid.Y)                  
    result.Add((RnaBase.U, RnaBase.A, RnaBase.A), None)               
    result.Add((RnaBase.U, RnaBase.A, RnaBase.G), None)               
    result.Add((RnaBase.U, RnaBase.G, RnaBase.U), Some AminoAcid.C)                  
    result.Add((RnaBase.U, RnaBase.G, RnaBase.C), Some AminoAcid.C)                  
    result.Add((RnaBase.U, RnaBase.G, RnaBase.A), None)
    result.Add((RnaBase.U, RnaBase.G, RnaBase.G), Some AminoAcid.W)                   
    result.Add((RnaBase.C, RnaBase.U, RnaBase.U), Some AminoAcid.L)
    result.Add((RnaBase.C, RnaBase.U, RnaBase.C), Some AminoAcid.L)
    result.Add((RnaBase.C, RnaBase.U, RnaBase.A), Some AminoAcid.L)
    result.Add((RnaBase.C, RnaBase.U, RnaBase.G), Some AminoAcid.L)
    result.Add((RnaBase.C, RnaBase.C, RnaBase.U), Some AminoAcid.P)
    result.Add((RnaBase.C, RnaBase.C, RnaBase.C), Some AminoAcid.P)
    result.Add((RnaBase.C, RnaBase.C, RnaBase.A), Some AminoAcid.P)
    result.Add((RnaBase.C, RnaBase.C, RnaBase.G), Some AminoAcid.P)
    result.Add((RnaBase.C, RnaBase.A, RnaBase.U), Some AminoAcid.H)
    result.Add((RnaBase.C, RnaBase.A, RnaBase.C), Some AminoAcid.H)
    result.Add((RnaBase.C, RnaBase.A, RnaBase.A), Some AminoAcid.Q)
    result.Add((RnaBase.C, RnaBase.A, RnaBase.G), Some AminoAcid.Q)
    result.Add((RnaBase.C, RnaBase.G, RnaBase.U), Some AminoAcid.R)
    result.Add((RnaBase.C, RnaBase.G, RnaBase.C), Some AminoAcid.R)
    result.Add((RnaBase.C, RnaBase.G, RnaBase.A), Some AminoAcid.R)
    result.Add((RnaBase.C, RnaBase.G, RnaBase.G), Some AminoAcid.R)
    result.Add((RnaBase.A, RnaBase.U, RnaBase.U), Some AminoAcid.I)
    result.Add((RnaBase.A, RnaBase.U, RnaBase.C), Some AminoAcid.I)
    result.Add((RnaBase.A, RnaBase.U, RnaBase.A), Some AminoAcid.I)
    result.Add((RnaBase.A, RnaBase.U, RnaBase.G), Some AminoAcid.M)
    result.Add((RnaBase.A, RnaBase.C, RnaBase.U), Some AminoAcid.T)
    result.Add((RnaBase.A, RnaBase.C, RnaBase.C), Some AminoAcid.T)
    result.Add((RnaBase.A, RnaBase.C, RnaBase.A), Some AminoAcid.T)
    result.Add((RnaBase.A, RnaBase.C, RnaBase.G), Some AminoAcid.T)
    result.Add((RnaBase.A, RnaBase.A, RnaBase.U), Some AminoAcid.N)
    result.Add((RnaBase.A, RnaBase.A, RnaBase.C), Some AminoAcid.N)
    result.Add((RnaBase.A, RnaBase.A, RnaBase.A), Some AminoAcid.K)
    result.Add((RnaBase.A, RnaBase.A, RnaBase.G), Some AminoAcid.K)
    result.Add((RnaBase.A, RnaBase.G, RnaBase.U), Some AminoAcid.S)
    result.Add((RnaBase.A, RnaBase.G, RnaBase.C), Some AminoAcid.S)
    result.Add((RnaBase.A, RnaBase.G, RnaBase.A), Some AminoAcid.R)
    result.Add((RnaBase.A, RnaBase.G, RnaBase.G), Some AminoAcid.R)
    result.Add((RnaBase.G, RnaBase.U, RnaBase.U), Some AminoAcid.V)
    result.Add((RnaBase.G, RnaBase.U, RnaBase.C), Some AminoAcid.V)
    result.Add((RnaBase.G, RnaBase.U, RnaBase.A), Some AminoAcid.V)
    result.Add((RnaBase.G, RnaBase.U, RnaBase.G), Some AminoAcid.V)
    result.Add((RnaBase.G, RnaBase.C, RnaBase.U), Some AminoAcid.A)
    result.Add((RnaBase.G, RnaBase.C, RnaBase.C), Some AminoAcid.A)
    result.Add((RnaBase.G, RnaBase.C, RnaBase.A), Some AminoAcid.A)
    result.Add((RnaBase.G, RnaBase.C, RnaBase.G), Some AminoAcid.A)
    result.Add((RnaBase.G, RnaBase.A, RnaBase.U), Some AminoAcid.D)
    result.Add((RnaBase.G, RnaBase.A, RnaBase.C), Some AminoAcid.D)
    result.Add((RnaBase.G, RnaBase.A, RnaBase.A), Some AminoAcid.E)
    result.Add((RnaBase.G, RnaBase.A, RnaBase.G), Some AminoAcid.E)
    result.Add((RnaBase.G, RnaBase.G, RnaBase.U), Some AminoAcid.G)
    result.Add((RnaBase.G, RnaBase.G, RnaBase.C), Some AminoAcid.G)
    result.Add((RnaBase.G, RnaBase.G, RnaBase.A), Some AminoAcid.G)
    result.Add((RnaBase.G, RnaBase.G, RnaBase.G), Some AminoAcid.G)
    result

let private RnaCodonTable = initCodonTable 

let public codonToAminoAcid (c: Codon) = RnaCodonTable.[c]


let public rnaBaseSeqToProteinSeq (rna: seq<RnaBase>) = 
    let rec translateUntilStopOrEmpty (acc: AminoAcid list) rna' = 
        match rna' with
        | [] -> (List.rev acc, [])
        | r1::r2::r3::tl -> let aa = codonToAminoAcid (r1,r2,r3) 
                            match aa with
                            | None -> (List.rev acc, tl)
                            | Some x -> translateUntilStopOrEmpty (x::acc) tl
        | _ -> failwith "Unexpected number of bases"
    let rec translate acc rna' =  
        match translateUntilStopOrEmpty [] rna' with
        | (proteinString, []) -> List.rev (proteinString :: acc)
        | (proteinString, tl) -> translate (proteinString ::acc) tl
    translate [] (List.ofSeq rna)
