// http://rosalind.info/problems/prot
open System.IO

open Dna


[<EntryPoint>]
let main argv = 
    let lines = seq {
        use f = File.OpenText("rosalind_grph.txt")

        let line = ref (f.ReadLine())

        while (!line <> null) do
            yield !line
            line := f.ReadLine()
    }

    let records = Rosalind.fasta lines

    let idPrefixSuffix n = seq {
        for fr in records do
           let id = fst fr
           let arr = Array.ofSeq (snd fr)
           let prefix = arr.[0..2]
           let suffix = arr.[(arr.Length - 3)..]
           yield (id, prefix, suffix)
    }

    let findMatches idPrefixSuffixSeq = 
        let arr = Array.ofSeq idPrefixSuffixSeq
        seq {
            for (id, pref, suff) in idPrefixSuffixSeq do
                for (id2, pref2, suff2) in idPrefixSuffixSeq do
                    if suff = pref2 && id <> id2 then
                        yield (id, id2)
        }

    let matches = idPrefixSuffix 3 |> findMatches

    use output = File.CreateText("results.txt")

    for mtch in matches do
        output.WriteLine("{0} {1}", (fst mtch), (snd mtch)) 

    0 // return an integer exit code
