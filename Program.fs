// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System

open FSharp.Charting
open System.Text.RegularExpressions

open FSharp.Data

// It is easier to work with Option types so we make everything options (even where there is no missing for certain fields like Name
// It would be nice if there was an easier way to do this 
type DataSet = CsvProvider<     "train.csv", 
                                Schema="Ticket=int option, Age=int option, Survived=bool option,Name=string option, Fare=float option, Parch=int option, Sex=string option, SibSp=int option, PClass=int option, PassengerId=int option", 
                                PreferOptionals=true                                >

type Passenger = DataSet.Row
//type test = CsvProvider<"train.csv",>


//(p : Passenger).Tick

// =======================================================
// Helpers
// =======================================================

module Seq =
   
    let normalizeBy foo (x : Option<'a> seq) =

        let (min,max) =  x  |> Seq.choose (fun n -> n) 
                            |> Seq.map foo
                            |> fun n -> (Seq.min n, Seq.max n)

        x |> Seq.map    ( function
                            | Some(z) -> 2.0*((foo z) - min)/(max - min) - 1.0
                            | None -> 0.0 
                        )

    let normalize (x : Option<float> seq) = normalizeBy (fun n -> n) x

let idFunc = (fun n -> n)

// =======================================================
// Predictor and tester
// =======================================================

let survivalPredictor (p : Passenger) =
    p.Sex 
        |> function | Some("female") -> Some(true) | _ -> Some(false)


let testPredictor predictor (ps : Passenger seq) =
    Seq.sumBy (fun (p : Passenger) -> if predictor p = p.Survived then 1 else 0) ps
        |> float
        |> fun n -> n / float(Seq.length ps)

// =======================================================
// Main
// =======================================================  

[<EntryPoint>]
let main argv = 

    let data = new DataSet()

    // We count the number of missing information per column
    // This shoudl work for any comma delimited CSV where all entries are of type Option<T>
    // Note that since we are comma delimited we need to specifically ignore
    // the comma inside the quotationts using a Regex, which is the case for Names

    let PassengerToIsSomeArray (p : Passenger) =
        p |> (sprintf "%A")
          |> fun str -> Regex.Replace(str, "[\"]([^,]+),([^,]+)[\"]", "<name>")
          |> fun s -> s.Split ','
          |> Seq.map (fun str -> if str.Contains("null") then 1 else 0)

    data.Rows
    |> Seq.map PassengerToIsSomeArray
    |> Seq.reduce (Seq.map2 (+))
    |> Seq.zip data.Headers.Value
    |> Seq.toArray
    |> Seq.choose (fun (a,b) -> if b <> 0 then Some((a,b)) else None)
    |> printfn "%A"

    // Which shows us (Field, # missing)
    // seq [("Age", 202); ("Ticket", 230); ("Cabin", 687)]
    // To get the most out of our data we would want to imputate the missing values

    // Eventually we will want to try to predict age based on the other complete features
    // First we will probably want to engineer some features from the complete set

    // Feature engineering based off the names

    let getTitle (p : Passenger) =
        match p.Name with
        | Some(name) -> Some <| Regex.Match(p.Name.Value, "(\w+)(\.)").Value
        | None -> None

    let isCommonTitle = function
                        | "Mr." -> true
                        | "Mrs." -> true
                        | "Miss." -> true
                        | "Master." -> true
                        | _ -> false

    data.Rows
    |> Seq.map getTitle
    |> Seq.countBy (fun n -> n.Value |> isCommonTitle )
    |> printfn "%A"
    
    data.Rows |> Seq.map getTitle |> Seq.distinct |> Seq.toArray
        |> Array.map (function | Some(x) -> x | None -> "?")
        |> printfn "%A"
    // We see that the most common titles are "Mr.", "Mrs.", "Miss." and "Master."
    // there are only 27 in the train.csv data that do not have either of those titles
//
//    let nameToTitle x = match x with
//                        | "Don." -> true
//                        | "Rev." -> true
//                        | "Dr."  -> true
//                        | "Mme." -> true
//                        | "Ms." 
//                        | "Major."
//                        | "Lady."
//                        | "Sir."
//                        | "Mlle."
//                        | "Col."
//                        | "Capt."
//                        | "Countess."
//                        | "Jonkheer."
//
//    [|"Mr."; "Mrs."; "Miss."; "Master."; "Don."; "Rev."; "Dr."; "Mme."; "Ms.";
//  "Major."; "Lady."; "Sir."; "Mlle."; "Col."; "Capt."; "Countess."; "Jonkheer."|]

  
    0 // return an integer exit code
