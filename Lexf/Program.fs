open Rosalind
open Permutations

open System.IO

let makeKey (lst: int list) = 
   let max = (Seq.length).ToString().Length
   let format = "{0:" + new System.String('0', max) + "}"
   let arr = Seq.map (fun (x: int) -> x.ToString(format)) lst |> Seq.toArray 
   System.String.Join("",arr)
  

// http://rosalind.info/problems/lexf/
[<EntryPoint>]
let main argv = 
    
    use f = File.OpenText("rosalind_lexf.txt")

    let alphabet= f.ReadLine().Split([|" "|], System.StringSplitOptions.RemoveEmptyEntries)
    
    
    let n = System.Int32.Parse(f.ReadLine())
    
    let indicesToString (indices: seq<int>) = 
        System.String.Join("", Seq.map (fun x -> Seq.nth x alphabet) indices |> Seq.toArray)

    let perms = Permutations.getPermsWithRep2 n [0 .. ((Seq.length alphabet) - 1)] 
    
    let sorted = Seq.sortBy makeKey perms

    let result = Seq.map indicesToString sorted 

    use output = File.CreateText("result.txt")

    for s in result do output.WriteLine(s)
        

    0 // return an integer exit code
