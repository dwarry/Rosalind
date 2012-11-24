module Kmp

open Microsoft.FSharp.Collections.Array.Parallel


///<summary>
///  Prepare the failure array of substring matches within <paramref name="data" />.
///  The value at a position <var>i</var> in the failure array is the number of items 
///  up to and including <var>i</var> that match the first <var>i</var> items at the
///  start of the sequence.
///</summary>
///<remarks>
///From http://www.inf.fh-flensburg.de/lang/algorithmen/pattern/kmpen.htm 
///<code><![DATA[
///    void kmpPreprocess()
///    {
///        int i=0, j=-1;
///        b[i]=j;
///        while (i<m)
///        {
///            while (j>=0 && p[i]!=p[j]) j=b[j];
///            i++; j++;
///            b[i]=j;
///        }
///    }
///]]></code>
///</remarks>
let public makeFailureArray (data: seq<'T>) = 
    let i = ref 0
    let j = ref -1
    let arr = Seq.toArray data

    let b = Array.zeroCreate<int> (arr.Length + 1)
    Array.set b !i !j
    while !i < arr.Length do
        while !j >= 0 && arr.[!i] <> arr.[!j] do j := b.[!j]
        i := !i + 1
        j := !j + 1
        Array.set b !i !j

    b.[1..arr.Length]



///<summary>
///  Finds the longest substring of <paramref name="pattern" /> that exists in
///  <paramref name="test" />.
///</summary>
///<returns>
///  A pair containing the longest substring, and the position in 
///  <paramref name="test"/> at which it was found. 
///</returns>
let public findLongestSubstringMatch (pattern: seq<'T>) (others: seq<seq<'T>>)=
    // the sequence we will use to find potential matches
    let test = Seq.head others

    // the other sequences that must match.
    let rest = Seq.skip 1 others |> Seq.toArray
    
    // For a potential match to be valid, it must be present in the other sequences.
    // To determine this, we prepend the potential match with each of the other sequences, 
    // determine the failure array for this longer sequence, and then test to see if any 
    // values in the failure array have the same value as the length of the potential
    // match.
    let othersHaveMatch (pattern': seq<'T>) = 
        let patternLength = Seq.length pattern'
        
        let otherFailureArrays =  Array.Parallel.map (fun (x: seq<'T>) -> Seq.append pattern' x 
                                                                          |> makeFailureArray 
                                                                          |> Array.toSeq) 
                                                                          //|> Seq.skip patternLength) 
                                                      rest
                                 
        otherFailureArrays |> Seq.forall (fun fa -> Seq.exists (fun x -> x = patternLength) fa) 
        
    // Find the longest match from the start of the pattern sequence that occurs within the 
    // test sequence.
    //
    // Returns a pair containing the potential match, and the position at which it was found.
    let findLongest (pattern' : seq<'T>) = 
        if Seq.isEmpty pattern' || Seq.isEmpty test then
            (Seq.empty<'T>, -1)
        else
            let failureArray = Seq.append pattern' test |> makeFailureArray
            
            let longestMatch =
                failureArray 
                |> Array.fold (fun st x -> if x > (fst st) && (othersHaveMatch (Seq.take x pattern') ) then 
                                               (x, (snd st) + 1) 
                                           else 
                                               ((fst st), (snd st) + 1))
                              (0,-1) 
            
            match longestMatch with
            | (0, _) -> (Seq.empty<'T>, -1)
            | (n, pos) -> (Seq.take n pattern', pos)

    
    let skips = seq { 0 .. (Seq.length pattern) - 1 }
    
    // Sequence of the initial pattern with successive leading characters removed.
    // Seeing that we will find the longest match from the start, we only need to 
    // make one pass through this sequence to cover all the possibilities.
    let allPatterns = Seq.map (fun x -> Seq.skip x pattern |> Seq.toArray) skips |> Seq.toList
                      
    // Process the list of patterns, finding the longest match that occurs in all the
    // other sequences. When we have found a match, we can ignore the next (length-of-pattern)
    // possibilities as these will just be shorter matches. 
    let rec processPatterns (patterns: 'T[] list) (currentLongestMatch: #seq<'T>) (ignoreNext: int) = 
        let currentLength = Seq.length currentLongestMatch
        let patternLength = Seq.length <| Seq.head patterns
        
        match patterns with
        | [] -> currentLongestMatch
        | hd :: tl when ignoreNext > 0 ->
            processPatterns tl currentLongestMatch (ignoreNext - 1)
        | hd :: tl when (Array.length hd) > currentLength ->
            
            match findLongest (Array.toSeq hd) with
            | (m, _) -> let newLongest = Seq.toArray m
                        if Array.length newLongest > currentLength then
                            
                            printfn " match length %d" (Seq.length m) 
                            processPatterns tl newLongest (Array.length newLongest)
                        else
                            processPatterns tl currentLongestMatch 0
        | _ -> currentLongestMatch
   
    processPatterns allPatterns Array.empty<'T> 0

