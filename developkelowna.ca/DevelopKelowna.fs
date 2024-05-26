module DevelopKelowna

open FSharp.Data
open Microsoft.FSharp.Reflection

type MapJson =
    JsonProvider<"https://developkelowna.ca/map/data?ne_lat=49.979&ne_lng=-119.199&sw_lat=49.792&sw_lng=-119.792">

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

type Project =
    { Address: string
      Developer: Option<string>
      SalesStatus: Option<SalesStatus>
      Status: Status
      Year: Option<int>
      Storeys: int
      Units: Option<int>
      Website: Option<string> }

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
        ProjectHtml
            .Load($"https://developkelowna.ca/projects/{name}")
            .Html.CssSelect(".pt-2 > .mt-3")

    let fields =
        project
        |> List.map flattenHtmlNode
        |> List.collect (function
            | [ "Address:"; address ] -> [ Address address ]
            | [ "Developer:"; developer ] -> [ Developer developer ]
            | [ "Sales Status:"; salesStatus ] -> [ SalesStatus.fromString salesStatus |> Field.SalesStatus ]
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
            | xs -> failwith $"Invalid fields %A{xs}")

    let address =
        fields
        |> List.choose (fun x ->
            match x with
            | Address a -> Some a
            | _ -> None)
        |> List.exactlyOne

    let developer =
        fields
        |> List.choose (fun x ->
            match x with
            | Developer d -> Some d
            | _ -> None)
        |> exactlyOneOrNone

    let status =
        fields
        |> List.choose (fun x ->
            match x with
            | Status s -> Some s
            | _ -> None)
        |> List.exactlyOne

    let year =
        fields
        |> List.choose (fun x ->
            match x with
            | Year y -> Some y
            | _ -> None)
        |> exactlyOneOrNone

    let storeys =
        fields
        |> List.choose (fun x ->
            match x with
            | Storeys s -> Some s
            | _ -> None)
        |> List.exactlyOne

    let units =
        fields
        |> List.choose (fun x ->
            match x with
            | Units u -> Some u
            | _ -> None)
        |> exactlyOneOrNone

    let website =
        fields
        |> List.choose (fun x ->
            match x with
            | Website w -> Some w
            | _ -> None)
        |> exactlyOneOrNone

    let salesStatus =
        fields
        |> List.choose (fun x ->
            match x with
            | SalesStatus s -> Some s
            | _ -> None)
        |> exactlyOneOrNone

    { Address = address
      Developer = developer
      SalesStatus = salesStatus
      Status = status
      Year = year
      Storeys = storeys
      Units = units
      Website = website }

[<EntryPoint>]
let main _ =
    let projects = MapJson.GetSample().Projects |> Array.map (_.Slug >> getProject)
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
          x.SalesStatus |> Option.map (fun x -> x.ToString()) |> Option.defaultValue ""
          x.Status.ToString()
          x.Year |> Option.map string |> Option.defaultValue ""
          x.Storeys.ToString()
          x.Units |> Option.map string |> Option.defaultValue ""
          x.Website |> Option.defaultValue "" ]
        |> String.concat delimiter)
    |> Array.iter (fun x -> x |> sw.WriteLine)

    sw.Close()

    0
