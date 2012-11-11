// Solution to problem http://rosalind.info/problems/prob/
open System.IO

let parseLine (line:string) = 
    let values = line.Split(' ')
    seq{ for v in values do yield System.Double.Parse(v) }


let calculateProbabilitiesOfTwoConsecutiveBases (gcValues: seq<float>) = 
    let gcToProbability gc = 
        let pA = (1.0 - gc) / 2.0
        let pC = gc / 2.0
        2.0 * ((pA * pA) + (pC * pC))
    Seq.map gcToProbability gcValues


[<EntryPoint>]
let main argv = 
    use f = File.OpenText("rosalind_prob.txt")
    let line = f.ReadLine()
//    let line = "0.23 0.31 0.75"
    let probabilities = (parseLine line |> calculateProbabilitiesOfTwoConsecutiveBases) 
    let results = Seq.map (fun x -> sprintf "%1.3f" x) probabilities
    printfn "%s" (System.String.Join(" ", results))
    0 // return an integer exit code
