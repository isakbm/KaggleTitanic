// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System

open System.Text.RegularExpressions



module Titanic =

    type Passenger =
        {
            PassengerId : Option<int>     
            Survived    : Option<bool>        
            Pclass      : Option<int>          
            Name        : string       
            Sex         : string         
            Age         : Option<int>              
            SibSp       : Option<int>             
            Parch       : Option<int>              
            Ticket      : string          
            Fare        : Option<float>            
            Cabin       : string      
            Embarked    : Option<char>    
        } 
    let ToOption (b : bool, p) = if b then Some(p) else None
    let Passenger (s : string seq) = 
        {
            PassengerId = Seq.item 0  s |> System.Int32.TryParse |> ToOption     
            Survived    = Seq.item 1  s |> System.Int32.TryParse |> ToOption |> function | Some(1) -> Some(true) | Some(0) -> Some(false) | _ -> None 
            Pclass      = Seq.item 2  s |> System.Int32.TryParse |> ToOption            
            Name        = Seq.item 3  s                            
            Sex         = Seq.item 4  s                              
            Age         = Seq.item 5  s |> System.Int32.TryParse |> ToOption           
            SibSp       = Seq.item 6  s |> System.Int32.TryParse |> ToOption            
            Parch       = Seq.item 7  s |> System.Int32.TryParse |> ToOption          
            Ticket      = Seq.item 8  s                               
            Fare        = Seq.item 9  s |> System.Double.TryParse|> ToOption          
            Cabin       = Seq.item 10 s                                
            Embarked    = Seq.item 11 s |> System.Char.TryParse  |> ToOption    
        }   

    let Survived (p : Passenger) = 
        match p.Survived with
        | Some(true) -> Some(true)
        | _ -> None
    
    let Sex p = p.Sex
    let Age p = p.Age
    let Fare p = p.Fare
    let Cabin p = p.Cabin
 

open Titanic 
    
[<EntryPoint>]
let main argv = 
    
    // Encapsulating the data in a nice way

    let lines = System.IO.File.ReadLines("train.csv")
    let head  = Seq.head lines
    
    let replaceCommasInSubstrings s : string =
        Regex.Replace(s, @"[""].+[""]", fun (m : Match) -> String.filter ((<>) ',') m.Value )  

    let names = head.Split ','
    let data  =
        Seq.tail lines
        |> Seq.map replaceCommasInSubstrings
        |> Seq.map (fun n -> n.Split ',')

    let pData = seq {for a in data do yield Titanic.Passenger(a)}


    Seq.choose Survived pData |> Seq.length |> printfn "%d"

    let females = 
        pData
        |> Seq.groupBy Sex
        |> Seq.find (fun (n,_) -> n = "female")
        |> snd

    let males = 
        pData
        |> Seq.groupBy Sex
        |> Seq.find (fun (n,_) -> n = "male")
        |> snd   

    [males; females]
        |> Seq.map (Seq.choose Survived)
        |> Seq.map Seq.length
        |> printfn "%A"
    
    
    0 // return an integer exit code
