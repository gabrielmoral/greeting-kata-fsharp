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

let greeting = sprintf "Hello, %s"
let addicionalGreeting = sprintf " AND HELLO %s"


let shoutGreeting greeting name = 
    let (greeting:string) = greeting name
    greeting.ToUpper()

let greet' greeting names =
    let composeMultipleNames names lastConjunction = 
        let reversedNames = names |> List.rev
        lastConjunction + reversedNames.Head :: reversedNames.Tail |> List.rev

    let (normalNames:string list), (shoutNames:string list) = names
    
    let normalGreeting = match normalNames with
                            |[] -> ""
                            |[_] -> greeting normalNames.Head
                            |[_;_] -> greeting (String.Join("", composeMultipleNames normalNames " and "))
                            |_ -> greeting (String.Join(", ", composeMultipleNames normalNames "and "))

    let shoutGreeting = match shoutNames, normalNames with
                        |[],_ -> ""
                        |[_], [] -> shoutGreeting greeting shoutNames.Head
                        |[_], _::_ -> shoutGreeting addicionalGreeting shoutNames.Head
                        |[_;_],_ -> shoutGreeting addicionalGreeting (String.Join("", composeMultipleNames shoutNames " and "))
                        |_ -> shoutGreeting addicionalGreeting (String.Join(", ", composeMultipleNames shoutNames "and "))

    normalGreeting + shoutGreeting

let greet receptor =
    let names = match receptor with
                |Simple name -> match name with
                                |"" -> ["my friend"]
                                |_ -> [name] 
                |Multiple names -> names

    greet' greeting (decompose names)

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

[<Fact>]
let ``Multiple greetings shouting`` () =
    Assert.Equal("Hello, Gabriel AND HELLO PEPE",greet(Multiple ["Gabriel"; "PEPE"]))
    Assert.Equal("Hello, Gabriel and Pepa AND HELLO PEPE",greet(Multiple ["Gabriel"; "PEPE"; "Pepa"]))
    Assert.Equal("Hello, Gabriel and Pepa AND HELLO PEPE AND PEPITO",greet(Multiple ["Gabriel"; "PEPE"; "Pepa"; "PEPITO"]))



//Hello, {Gabriel} and {Pepa} AND HELLO {PEPE} is a template, generatethe template?