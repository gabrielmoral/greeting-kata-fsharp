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

let private composeNames (names:string list) =
    let composeMultipleNames names lastConjunction = 
        let reversedNames = names |> List.rev
        lastConjunction + reversedNames.Head :: reversedNames.Tail |> List.rev

    if names.Length = 2 then 
        String.Join("", composeMultipleNames names " and ")
    else 
        String.Join(", ", composeMultipleNames names "and ")

let shoutGreeting greeting name = 
    let (greeting:string) = greeting name
    greeting.ToUpper()

let greet' greeting name = 
    match isUpper name with
    |true ->  shoutGreeting greeting name
    |false -> greeting name

let greet receptor =
    let greeting = sprintf "Hello, %s"
    let properName = match receptor with
                        |Simple name -> match name with
                                        |"" -> "my friend"
                                        |_ -> name 
                        |Multiple names -> composeNames names

    greet' greeting properName

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