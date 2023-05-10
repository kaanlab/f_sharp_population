open System
open System.IO
open FSharp.Data
open FsToolkit.ErrorHandling.ValidationCE

type Population = {
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

type ValidationError =
| MissingData of name: string

let removeComma(s: string) = s.Replace(",", "")
let toInt(s: string) = Int32.Parse(s)

let load (files: string[])=
    files
    |> Seq.map CsvFile.Load
    |> Seq.fold (fun acc file -> Seq.append acc file.Rows ) []

let parse (data:seq<CsvRow>) = 
    data
    |> Seq.choose (fun line -> 
        match line.Columns with
        | [| isoCode; country; lastUpdate; population; area; landArea; densitySq; growthRate; worldPersnt; rank|] -> 
            Some { 
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

let (|IsEmptyString|_|) (input:string) =
    if input.Trim() = "" then Some () else None

let (|IsNotInt|_|) (input:string) =
    let (success, _) = input |> Int32.TryParse
    if success then None else Some input

let validateInput (args:string) = 
    if args <> "" then Ok args
    else Error (MissingData "Invalid args")

let create isoInput = { IsoCode = isoInput }  

let validate (argv: string) : Result<ValidInput,ValidationError list> =
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

let populationHead(data: seq<Population>) =
    data
    |> populationSeq
    |> Seq.head 

let populationDelta(data:seq<Population>) = 
     data
    |> populationSeq
    |> Seq.skip 1
    |> Seq.reduce(fun acc i -> acc - i) 

let populationСhange(isoCode:Result<ValidInput,ValidationError list> ) (data: seq<Population>) =    
    data
    |> Seq.filter(fun p -> p.IsoCode.Equals(isoCode))
    |> Seq.groupBy (fun p -> p.IsoCode) 
    |> Seq.map (fun (key, value) -> { 
       IsoCode = key 
       PopulationDelta = populationDelta(value) 
    })

[<EntryPoint>]
let main argv =
    let isoCode = validate argv[0]
    let path = Path.Combine(__SOURCE_DIRECTORY__, "resources")
    load (Directory.GetFiles(path, "*.csv"))
    |> parse
    |> populationСhange isoCode
    |> Seq.iter (fun p -> printfn "%A" p)
    0