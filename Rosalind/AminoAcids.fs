module AminoAcids


type public AminoAcid = |A|C|D|E|F|G|H|I|K|L|M|N|P|Q|R|S|T|V|W|Y


let private initAminoAcidMasses = 
    let result = new System.Collections.Generic.Dictionary<AminoAcid, float>()
    result.Add(AminoAcid.A, 71.03711   )
    result.Add(AminoAcid.C, 103.00919  )
    result.Add(AminoAcid.D, 115.02694  )
    result.Add(AminoAcid.E, 129.04259  )
    result.Add(AminoAcid.F, 147.06841  )
    result.Add(AminoAcid.G, 57.02146   )
    result.Add(AminoAcid.H, 137.05891  )
    result.Add(AminoAcid.I, 113.08406  )
    result.Add(AminoAcid.K, 128.09496  )
    result.Add(AminoAcid.L, 113.08406  )
    result.Add(AminoAcid.M, 131.04049  )
    result.Add(AminoAcid.N, 114.04293  )
    result.Add(AminoAcid.P, 97.05276   )
    result.Add(AminoAcid.Q, 128.05858  )
    result.Add(AminoAcid.R, 156.10111  )
    result.Add(AminoAcid.S, 87.03203   )
    result.Add(AminoAcid.T, 101.04768  )
    result.Add(AminoAcid.V, 99.06841   )
    result.Add(AminoAcid.W, 186.07931  )
    result.Add(AminoAcid.Y, 163.06333  )
    result

let private AminoAcidMasses = initAminoAcidMasses

let public AminoAcidToChar = 
    function
    | AminoAcid.A -> 'A' | AminoAcid.C -> 'C' | AminoAcid.D -> 'D' | AminoAcid.E -> 'E'
    | AminoAcid.F -> 'F' | AminoAcid.G -> 'G' | AminoAcid.H -> 'H' | AminoAcid.I -> 'I'
    | AminoAcid.K -> 'K' | AminoAcid.L -> 'L' | AminoAcid.M -> 'M' | AminoAcid.N -> 'N'
    | AminoAcid.P -> 'P' | AminoAcid.Q -> 'Q' | AminoAcid.R -> 'R' | AminoAcid.S -> 'S'
    | AminoAcid.T -> 'T' | AminoAcid.V -> 'V' | AminoAcid.W -> 'W' | AminoAcid.Y -> 'Y'

let public charToAminoAcid =
    function 
        | 'A' -> AminoAcid.A | 'C' -> AminoAcid.C | 'D' -> AminoAcid.D | 'E' -> AminoAcid.E 
        | 'F' -> AminoAcid.F | 'G' -> AminoAcid.G | 'H' -> AminoAcid.H | 'I' -> AminoAcid.I 
        | 'K' -> AminoAcid.K | 'L' -> AminoAcid.L | 'M' -> AminoAcid.M | 'N' -> AminoAcid.N 
        | 'P' -> AminoAcid.P | 'Q' -> AminoAcid.Q | 'R' -> AminoAcid.R | 'S' -> AminoAcid.S 
        | 'T' -> AminoAcid.T | 'V' -> AminoAcid.V | 'W' -> AminoAcid.W | 'Y' -> AminoAcid.Y
        | _ -> failwith "unknown amino acid"

let public proteinStringToSeq (p: string) =
    seq { for aa in p do yield charToAminoAcid aa }


let public proteinSeqToString (p: seq<AminoAcid>) = 
     new System.String(Seq.map AminoAcidToChar p |> Seq.toArray)   

let public calculateMassOfProteinSeq (ps: seq<AminoAcid>) =
    Seq.sumBy (fun aa -> AminoAcidMasses.[aa]) ps
