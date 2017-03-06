// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open FSharp.Data



type Stocks = CsvProvider<"../data/MSFT.csv", ",">

[<EntryPoint>]
let main argv = 
    
    let msft = Stocks.Load("http://ichart.finance.yahoo.com/table.csv?s=MSFT")

 

    msft |> printfn "%A"
 
    for row in msft.Rows do
        row |> printfn "%A"


    
    
    
    0 // return an integer exit code
    