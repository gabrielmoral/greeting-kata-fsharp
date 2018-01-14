module Tests

open System
open Xunit

type Receptor =
| Simple of string
| Multiple of string list

let private isUpper (name:string) =
    let rec checkUpper isUpper arrayName =
        match arrayName with
        | [] -> isUpper
        | _ -> if Char.IsUpper arrayName.Head then checkUpper true arrayName.Tail
                else false
    
    checkUpper false (name.ToCharArray() |> Array.toList)

let private decompose (names:string list) =
    names |> List.filter (isUpper >> not),  names |> List.filter isUpper

let hello name = 
    let greeting = sprintf "Hello%s" name
    if name.Replace(", ","") |> isUpper then greeting.ToUpper()
    else greeting

let namef = sprintf ", %s"

let and' = sprintf "%s and "

let shoutGreeting greeting name = 
    let (greeting:string) = greeting name
    greeting.ToUpper()

let greet' (names:string list) =
    let rec compose greeting (rest:string list) n =
        
        let createGreeting name = compose (namef name)

        let createLastGreeting name lastName =
            if n > 2 then createGreeting (name + ", and " + lastName) 
            else createGreeting (name + " and " + lastName) 

        let greeting' = match rest with
                        |[]-> ""
                        |[name] -> createGreeting name rest.Tail n
                        |name::tail-> match tail with   
                                        |[lastName] -> createLastGreeting name lastName [] n
                                        |_ -> createGreeting name tail n
        
        match greeting with
        | "" -> hello greeting'
        | _ -> greeting + greeting'

    compose "" names names.Length

let greet receptor =
    let names = match receptor with
                |Simple name -> match name with
                                |"" -> ["my friend"]
                                |_ -> [name] 
                |Multiple names -> names

    greet' names

[<Fact>]
let ``Hello name`` () =
    Assert.Equal("Hello, Gabriel",greet(Simple "Gabriel"))
    Assert.Equal("Hello, my friend",greet(Simple ""))

[<Fact>]
let ``Shout greeting`` () =
    Assert.Equal("HELLO, GABRIEL",greet(Simple "GABRIEL"))

[<Fact>]
let ``Multiple greetings`` () =
    Assert.Equal("Hello, Gabriel and Pepe",greet(Multiple ["Gabriel"; "Pepe"]))
    Assert.Equal("Hello, Gabriel, Pepe, and Pepa",greet(Multiple ["Gabriel"; "Pepe"; "Pepa"]))

// [<Fact>]
// let ``Multiple greetings shouting`` () =
//     Assert.Equal("Hello, Gabriel AND HELLO PEPE",greet(Multiple ["Gabriel"; "PEPE"]))
//     Assert.Equal("Hello, Gabriel and Pepa AND HELLO PEPE",greet(Multiple ["Gabriel"; "PEPE"; "Pepa"]))
//     Assert.Equal("Hello, Gabriel and Pepa AND HELLO PEPE AND PEPITO",greet(Multiple ["Gabriel"; "PEPE"; "Pepa"; "PEPITO"]))



//Hello, {Gabriel} and {Pepa} AND HELLO {PEPE} is a template, generatethe template?