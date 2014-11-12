open System
open System.IO
open System.Collections.Generic

// the following might come in handy: 
//File.ReadAllLines(path)
// returns an array of strings for each line 
 
//let path = let lines = File.ReadAllLines @"C:\Users\Tobias\Documents\Repos\Dojo-Digits-Recognizer\Dojo\"


let validationFile = @"C:\Users\Tobias\Documents\Repos\Dojo-Digits-Recognizer\Dojo\validationsample.csv"
let trainingFile = @"C:\Users\Tobias\Documents\Repos\Dojo-Digits-Recognizer\Dojo\trainingsample.csv"

type Digit = { Label:int; Pixels:int[] }

let getData fileWithPath = 
    let lines = File.ReadAllLines fileWithPath
    let stringFieldsWithHeaders = lines |> Array.map (fun x -> x.Split(',')) 
 
    let stringFields = stringFieldsWithHeaders.[1 .. ]

    let intFields = stringFields |> Array.map (fun x -> x |> Array.map Convert.ToInt32 )
 
    let recordfields = intFields |> Array.map (fun x -> {
                                                            Label = x.[0]
                                                            Pixels = x.[1..]
                                                        }
                                              )
    recordfields

let recordFields = getData trainingFile
let testFields = getData validationFile
 
let square x = x*x
let distance (p1:int[]) (p2:int[]) :float = 
    (p1, p2) ||> Array.map2 (fun p1 p2 -> square (p1 - p2) ) 
             |> Array.sum
             |> float
             |> sqrt

// 1 means first nearest N.
let classifyKnn k (unknown:int[]) =
    let result = recordFields |> Array.map (fun x -> (x.Label, distance x.Pixels unknown))
                              |> Array.sortBy snd
    result.[..(k-1)]
 
let classify = classifyKnn 1

classify recordFields.[10].Pixels

let voting (results:(int*float)[]) =
    let updateDict (key: int) (dict: Dictionary<int, int>) : unit = 
        if dict.ContainsKey key then
            dict.[key] <- dict.[key] + 1
        else
            dict.Add(key, 1)

    results |> Array.fold (fun acc res -> updateDict (fst res) acc; acc) (new Dictionary<int,int>())

let classifyRecords k = Array.Parallel.map (fun record -> ((classifyKnn k record.Pixels), record.Label)) 

//recordFields |> Array.Parallel.map (fun record -> ( (classify record.Pixels).[0] |> fst , record.Label) ) 
//             |> Array.forall (fun (x, y) -> x = y)

let test votes = votes |> Array.map (fun (x, y) -> if x = y then 1 else 0) |> Array.sum

//let good = testFields |> classifyRecords 1
//                      |> Array.map (fun (gss, l) -> (fst gss.[0], l))
//                      |> test
//
//let percent = float(good) / float(testFields.Length)

let votingTest = testFields |> classifyRecords 3 
                            |> Array.map (fun (lgss, l) -> lgss |> voting 
                                                                |> Seq.maxBy (fun kvp -> kvp.Value)
                                                                |> fun kvp -> (kvp.Key, kvp.Value))

let voteingPercent = float(test votingTest) / float(testFields.Length)



