// http://rosalind.info/problems/kmp
open System.IO

open Dna

[<EntryPoint>]
let main argv = 
        
    use f = File.OpenText("rosalind_kmp.txt")

    use output = File.CreateText("results.txt")

    
    let kmp (dna: DnaBase[]) = 
    (* From http://www.inf.fh-flensburg.de/lang/algorithmen/pattern/kmpen.htm 
    void kmpPreprocess()
    {
        int i=0, j=-1;
        b[i]=j;
        while (i<m)
        {
            while (j>=0 && p[i]!=p[j]) j=b[j];
            i++; j++;
            b[i]=j;
        }
    }
     *)
        let i = ref 0
        let j = ref -1

        let b = Array.zeroCreate<int> (dna.Length + 1)
        Array.set b !i !j
        while !i < dna.Length do
            while !j >= 0 && dna.[!i] <> dna.[!j] do j := b.[!j]
            i := !i + 1
            j := !j + 1
            Array.set b !i !j

        b.[1..dna.Length]

    let line = f.ReadLine()

    let dna = dnaStringToDnaBaseSeq line |> Array.ofSeq
    
    // C A G T A A G C A G G G A C T G 
    // 0 0 0 0 0 0 0 1 2 3 0 0 0 1 0 0
    
    // A A A C A A A A A A G C A T A A T T T G T C C A G C G C C C A C A C G C T G A
    // 0 1 2 0 1 2 3 3 3 3 0 0 1 0 1 2 0 0 0 0 0 0 0 1 0 0 0 0 0 0 1 0 1 0 0 0 0 0 1
    for result in kmp dna do 
        output.Write("{0} ", result)

    0