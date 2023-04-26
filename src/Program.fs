open System
open System.IO
open FSharp.Data

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

let removeComma(s: string) = s.Replace(",", "")
let toInt(s: string) = Int32.Parse(s)
let path = Path.Combine(__SOURCE_DIRECTORY__, "resources")
let files = Directory.GetFiles(path, "*.csv")

let load =
    files
    |> Seq.map CsvFile.Load
    |> Seq.fold (fun acc file -> Seq.append acc file.Rows ) []

let parse (data:seq<CsvRow>) = 
    data
    |> Seq.map (fun line -> 
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
    |> Seq.choose id

let populationSeq(data: seq<Population>) = 
    data
    |> Seq.map (fun p -> p.Population)     


let populationHead(data: seq<Population>) =
    data
    |> populationSeq
    |> Seq.head 

let populationDelta(data:seq<Population>) =    
    let head = populationHead(data)
    data
    |> populationSeq
    |> Seq.skip 1
    |> Seq.fold(fun acc i -> acc - i) head

let populationСhange(data: seq<Population>) =
    data
    |> Seq.groupBy (fun p -> p.IsoCode) 
    |> Seq.map (fun (key, value) -> { 
       IsoCode = key 
       PopulationDelta = populationDelta(value) 
    })

[<EntryPoint>]
let main argv =
    load
    |> parse
    |> populationСhange
    |> Seq.filter(fun p -> p.IsoCode.Equals(argv.[0]))
    |> Seq.iter (fun p -> printfn "%A" p)
    0