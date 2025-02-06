module DevelopKelowna

open FSharp.Data
open Microsoft.FSharp.Reflection

type MapJson =
    JsonProvider<
        "https://developkelowna.ca/map/data?ne_lat=49.9599&ne_lng=-119.1991&sw_lat=49.8116&sw_lng=-119.7923&status[]=0&status[]=3&status[]=1&status[]=2&type[]=1&type[]=2&type[]=3&type[]=4"
     >

type ProjectHtml = HtmlProvider<"https://developkelowna.ca/projects/1333-bertram">

type SalesStatus =
    | ``Now Selling``
    | ``Not yet announced``
    | ``Sold Out``
    | ``Register now``

    static member fromString x =
        match x with
        | "Now Selling" -> ``Now Selling``
        | "Sold Out" -> ``Sold Out``
        | "Not yet announced" -> ``Not yet announced``
        | "Register now" -> ``Register now``
        | _ -> failwith $"Invalid sales status {x}"

type Status =
    | Approved
    | Proposed
    | ``Under Construction``
    | Completed
    | ``On Hold``
    | Cancelled
    | Concept

    static member fromString(x) =
        match x with
        | "Proposed" -> Proposed
        | "Under Construction" -> ``Under Construction``
        | "Completed" -> Completed
        | "On Hold" -> ``On Hold``
        | "Cancelled" -> Cancelled
        | "Concept" -> Concept
        | "Approved" -> Approved
        | _ -> failwith $"Invalid status {x}"

type Field =
    | Address of string
    | Developer of string
    | Status of Status
    | Year of int
    | Storeys of int
    | Units of int
    | Website of string
    | SalesStatus of SalesStatus
    | Architect of string

type Project =
    { Address: string
      Developer: option<string>
      Architect: option<string>
      Status: option<Status>
      SalesStatus: option<SalesStatus>
      Year: option<int>
      Storeys: option<int>
      Units: option<int>
      Website: option<string> }

let rec flattenHtmlNode (node: HtmlNode) =
    match node.Elements() with
    | [] -> [ node.InnerText() ]
    | xs -> xs |> List.collect flattenHtmlNode

let exactlyOneOrNone (xs: 'a list) =
    match xs with
    | [ x ] -> Some x
    | [] -> None
    | _ -> failwith "Expected one or none"

let getProject (name: string) =
    let project =
        let p = ProjectHtml.Load($"https://developkelowna.ca/projects/{name}")
        let first = p.Html.CssSelect(".card-body.fs-5 > div")
        let second = p.Html.CssSelect(".card-body.pt-2 > div")
        first @ second

    let fields =
        project
        |> List.map flattenHtmlNode
        |> List.collect (function
            | [ "Address:"; address ] -> [ Address address ]
            | [ "Architect:"; architect ] -> [ Architect architect ]
            | [ "Developer:"; developer ] -> [ Developer developer ]
            | [ "Developer:"; developer; "Architect:"; architect ] -> [ Developer developer; Architect architect ]
            | "Sales Status:" :: salesStatus :: _ -> [ SalesStatus.fromString salesStatus |> Field.SalesStatus ]
            | [ "Status:"; status; "(Application file)" ]
            | [ "Status:"; status ] -> [ Status.fromString status |> Field.Status ]
            | [ "Status:"; status; year; "(Application file)" ]
            | [ "Status:"; status; year ] ->
                [ Status.fromString status |> Field.Status
                  (try
                      int year[4..]
                   with x ->
                       int year)
                  |> Year ]
            | [ "Storeys:"; storeys ] -> [ int storeys |> Storeys ]
            | [ "Storeys:"; storeys; "Units:"; units ] -> [ int storeys |> Storeys; int units |> Units ]
            | [ "Website:"; website ] -> [ Website website ]
            | [ "" ] -> []
            | [ ""; ""; ""; "" ] -> []
            | [ ""; ""; ""; ""; " 1 " ] -> []
            | [ "Storeys:"; storeys; "Units:"; units; "Parking:"; _ ] -> [ int storeys |> Storeys; int units |> Units ]
            | xs -> failwith $"Invalid fields %A{xs}")

    let address =
        fields
        |> List.choose (function
            | Address a -> Some a
            | _ -> None)
        |> List.exactlyOne

    let extractField f =
        fields |> List.choose f |> exactlyOneOrNone

    let developer =
        extractField (function
            | Developer d -> Some d
            | _ -> None)

    let architect =
        extractField (function
            | Architect a -> Some a
            | _ -> None)

    let status =
        extractField (function
            | Status s -> Some s
            | _ -> None)

    let salesStatus =
        extractField (function
            | SalesStatus s -> Some s
            | _ -> None)

    let year =
        extractField (function
            | Year y -> Some y
            | _ -> None)

    let storeys =
        extractField (function
            | Storeys s -> Some s
            | _ -> None)

    let units =
        extractField (function
            | Units u -> Some u
            | _ -> None)

    let website =
        extractField (function
            | Website w -> Some w
            | _ -> None)

    { Address = address
      Developer = developer
      Architect = architect
      Status = status
      SalesStatus = salesStatus
      Year = year
      Storeys = storeys
      Units = units
      Website = website }

[<EntryPoint>]
let main _ =
    let projects = MapJson.GetSample().Projects
    let projects = projects |> Array.map (_.Slug >> getProject)
    use sw = new System.IO.StreamWriter("projects.csv")
    let delimiter = ","

    let header =
        FSharpType.GetRecordFields(typeof<Project>)
        |> Array.map _.Name
        |> String.concat delimiter

    header |> sw.WriteLine

    projects
    |> Array.map (fun x ->
        [ $"\"{x.Address}\""
          x.Developer |> Option.defaultValue ""
          x.Architect |> Option.defaultValue ""
          x.Status |> Option.map (fun x -> x.ToString()) |> Option.defaultValue ""
          x.SalesStatus |> Option.map (fun x -> x.ToString()) |> Option.defaultValue ""
          x.Year |> Option.map string |> Option.defaultValue ""
          x.Storeys |> Option.map string |> Option.defaultValue ""
          x.Units |> Option.map string |> Option.defaultValue ""
          x.Website |> Option.defaultValue "" ]
        |> String.concat delimiter)
    |> Array.iter (fun x -> x |> sw.WriteLine)

    sw.Close()

    0
