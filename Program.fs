// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System

open System.Text.RegularExpressions



module Seq =
   

    let normalizeBy foo (x : Option<'a> seq) =

        let (min,max) =  x |> Seq.choose (fun n -> n) 
                                |> Seq.map foo
                                |> fun n -> (Seq.min n, Seq.max n)

        x |> Seq.map    ( function
                            | Some(z) -> 2.0*((foo z) - min)/(max - min) - 1.0
                            | None -> 0.0 
                        )

    let normalize (x : Option<float> seq) = normalizeBy (fun n -> n) x
        

module Titanic =

    type Passenger =
        {
            PassengerId : Option<int>     
            Survived    : Option<bool>        
            Pclass      : Option<int>          
            Name        : Option<string>        
            Sex         : Option<string>          
            Age         : Option<int>              
            SibSp       : Option<int>             
            Parch       : Option<int>              
            Ticket      : Option<int>          
            Fare        : Option<float>            
            Cabin       : Option<string>       
            Embarked    : Option<char>    
        } 

    let TryToOption (b : bool, p) = if b then Some(p) else None
    let strToOption (s : string)  = if s.Length = 0 then None else Some(s)
    let IntToBool = function 
                        | Some(1) -> Some(true) 
                        | Some(0) -> Some(false)
                        | _ -> None 
    

    let Passenger (s : string seq) = 
        {
            PassengerId = Seq.item 0  s |> System.Int32.TryParse |> TryToOption     
            Survived    = Seq.item 1  s |> System.Int32.TryParse |> TryToOption |> IntToBool
            Pclass      = Seq.item 2  s |> System.Int32.TryParse |> TryToOption            
            Name        = Seq.item 3  s |> strToOption                           
            Sex         = Seq.item 4  s |> strToOption                               
            Age         = Seq.item 5  s |> System.Int32.TryParse |> TryToOption           
            SibSp       = Seq.item 6  s |> System.Int32.TryParse |> TryToOption            
            Parch       = Seq.item 7  s |> System.Int32.TryParse |> TryToOption          
            Ticket      = Seq.item 8  s |> System.Int32.TryParse |> TryToOption                                        
            Fare        = Seq.item 9  s |> System.Double.TryParse|> TryToOption          
            Cabin       = Seq.item 10 s |> strToOption                                 
            Embarked    = Seq.item 11 s |> System.Char.TryParse  |> TryToOption    
        }   

    let Survived (p : Passenger) = 
        match p.Survived with
        | Some(true) -> Some(true)
        | _ -> None
    
    let Pclass p = p.Pclass
    let Sex p = p.Sex
    let Age p = p.Age
    let SibSp p = p.SibSp
    let Parch p = p.Parch
    let Ticket p = p.Ticket
    let Fare p = p.Fare
    let Cabin p = p.Cabin
    let Embarked p = p.Embarked
 

open Titanic 

let survivalPredictor (p : Passenger) =
    p.Sex 
        |> function | Some("female") -> Some(true) | _ -> Some(false)


let testPredictor predictor (ps : Passenger seq) =
    Seq.sumBy (fun (p : Passenger) -> if predictor p = p.Survived then 1 else 0) ps
        |> float
        |> fun n -> n / float(Seq.length ps)
  
[<EntryPoint>]
let main argv = 
    
    // Encapsulating the data in a nice way
    
    let replaceCommasInSubstrings s : string =
        Regex.Replace(s, @"[""].+[""]", fun (m : Match) -> String.filter ((<>) ',') m.Value )  

    let passengers  =
        System.IO.File.ReadLines("train.csv")
        |> Seq.tail
        |> Seq.map replaceCommasInSubstrings
        |> Seq.map (fun n -> n.Split ',')
        |> Seq.map (fun n -> Titanic.Passenger(n))
    
    // Normalizing passengers
    
    let normPclass =
        passengers
        |> Seq.map Pclass
        |> Seq.normalizeBy float
        |> fun n -> (Seq.min n, Seq.max n)
        |> printfn "%A"

    let normSexs =
        passengers
        |> Seq.map Sex
        |> Seq.map (function | Some("female") -> +1.0 | Some("male") -> -1.0 | _ -> 0.0)
        |> fun n -> (Seq.min n, Seq.max n)
        |> printfn "%A"
       
    let normSibSp =
        passengers
        |> Seq.map SibSp
        |> Seq.normalizeBy float
        |> fun n -> (Seq.min n, Seq.max n)
        |> printfn "%A"

    let normParch =
        passengers
        |> Seq.map Parch
        |> Seq.normalizeBy float
        |> fun n -> (Seq.min n, Seq.max n)
        |> printfn "%A"

    let normTickets =
        passengers
        |> Seq.map Ticket
        |> Seq.normalizeBy float
        |> fun n -> (Seq.min n, Seq.max n)
        |> printfn "%A"

    let normFares =
        passengers
        |> Seq.map Fare 
        |> Seq.normalize
        |> fun n -> (Seq.min n, Seq.max n)
        |> printfn "%A"

    let normEmbarks =
        passengers
        |> Seq.map Embarked
        |> Seq.normalizeBy (int >> float)
        |> fun n -> (Seq.min n, Seq.max n)
        |> printfn "%A"

//    let normCabins =
//        passengers
//        |> Seq.map Cabin
//        |> 
//
    let cabinFirstLetters = 
        passengers
        |> Seq.map Cabin
        |> Seq.map (function | Some(s) -> s.[0] | None -> '?')
        |> Seq.distinct
        |> Seq.toList
        |> List.skip 1


    let chooseCab (pas : Passenger seq) (c : char)  = 
                    pas
                    |> Seq.choose (fun p -> match p.Cabin with | Some(x) -> Some(p) | _ -> None)
                    |> Seq.choose (fun p -> if p.Cabin.Value.[0] = c then Some(p) else None)


    let percentSurvivorsAtCab (c : char) =
        let pasC =  chooseCab passengers c
        pasC
        |> Seq.map Survived
        |> Seq.sumBy (function | Some(true) -> 1 | _ -> 0)
        |> fun n -> float n/(float <| Seq.length pasC)
    
    List.map percentSurvivorsAtCab cabinFirstLetters |> printfn "%A"

    let survivorCounts =
        cabinFirstLetters
        |> List.map (fun n -> chooseCab passengers n |> Seq.length) 


    
//    let fares = Seq.map Fare passengers |> Seq.averageBy Option.get
//    printfn "%A" fares

    // Testing prediction

//    testPredictor survivalPredictor passengers |> printfn "Predictor accuracy = %f" 

        
    
    0 // return an integer exit code
