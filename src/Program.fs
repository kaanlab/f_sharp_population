open System
open System.IO
open FSharp.Data
open FsToolkit.ErrorHandling.ValidationCE

type Population = {
    Year: int
    IsoCode : string
    Country : string
    LastUpdated : int
    Population : int
    Area : string
    LandArea : string
    DensitySq : string
    GrowthRate : string
    WorldPersnt : string
    Rank : string
}

type Result = {
    IsoCode : string
    PopulationDelta : int
}

type ValidInput = { 
    IsoCode : string 
}

let removeComma(s: string) = s.Replace(",", "")
let toInt(s: string) = Int32.Parse(s)

let loadCSVData (filePath: string) : CsvFile =
    CsvFile.Load(filePath)

let load (files: string[])=
    files
    |> Seq.map CsvFile.Load

let getHeader(file:CsvFile) =
    match file.Headers with
    | Some(value) -> value.[2]
    | None -> ""

let parse (file:CsvFile) =     
    let header = getHeader file
    let year = header.[..3] |> toInt
    file.Rows 
    |> Seq.choose (fun line -> 
        match line.Columns with
        | [| isoCode; country; lastUpdate; population; area; landArea; densitySq; growthRate; worldPersnt; rank|] -> 
            Some {
                Year = 0
                IsoCode = isoCode
                Country = country
                LastUpdated = lastUpdate |> removeComma |> toInt
                Population = population |> removeComma |> toInt
                Area = area
                LandArea = landArea
                DensitySq = densitySq 
                GrowthRate = growthRate
                WorldPersnt = worldPersnt
                Rank = rank
            }
        | _ -> None
    )
    |> Seq.map(fun p -> { p with Year = year })

let (|IsEmptyString|_|) (input:string) =
    if input.Trim() = "" then Some () else None

let (|IsNotInt|_|) (input:string) =
    let (success, _) = input |> Int32.TryParse
    if success then None else Some input

let validateInput (args:string) = 
    if args <> "" then Ok args
    else Error "Invalid args"

let create isoInput = { IsoCode = isoInput }  

let validate (argv: string) =
  validation {
    let! isoCode = 
        argv 
        |> validateInput 
        |> Result.mapError (fun ex -> [ ex ])
    return create isoCode
  }

let populationSeq(data: seq<Population>) = 
    data
    |> Seq.map (fun p -> p.Population)     

let populationDelta(data:seq<Population>) = 
     data
    |> populationSeq
    |> Seq.reduce(fun acc i -> acc - i) 

let populationСhange (data: seq<Population>) =    
    data
    |> Seq.groupBy (fun p -> p.IsoCode) 
    |> Seq.map (fun (key, value) -> {
        IsoCode = key
        PopulationDelta = populationDelta(value)
        } 
    )

let populationСhangeByIso(isoCode:string ) (data: seq<Population>) =    
    data
    |> Seq.filter(fun p -> p.IsoCode.Equals(isoCode, StringComparison.OrdinalIgnoreCase))
    |> populationСhange

let getInputFromConsole (data:seq<Population>) =
    let codes = data |> Seq.map(fun p -> p.IsoCode)
    codes
    |> Seq.map (fun s -> s + " ")
    |> String.concat "   "
    |> printfn "%s"
    printfn "\nPlease type country name or ALL:"
    let input = Console.ReadLine()
    if input.Equals("all", StringComparison.OrdinalIgnoreCase) then
        populationСhange data
    elif(Seq.exists(fun x -> String.Equals(x, input, StringComparison.OrdinalIgnoreCase)) codes) then
        populationСhangeByIso input data 
    else
        seq{{ 
            IsoCode = "Not found"
            PopulationDelta = 0
        }}

[<EntryPoint>]
let main argv =
    let path = Path.Combine(__SOURCE_DIRECTORY__, "resources")
    load (Directory.GetFiles(path, "*.csv")) |> Seq.fold (fun acc file -> file |> parse) Seq.empty<Population>  
    |> getInputFromConsole    
    |> Seq.iter (fun p -> printfn "%A" p)    
    0