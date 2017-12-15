module Tests

open System
open Xunit

let private isUpper (name:string) =
    let rec checkUpper isUpper arrayName =
        match arrayName with
        | [] -> isUpper
        | _ -> if Char.IsUpper arrayName.Head then checkUpper true arrayName.Tail
                else false
    
    checkUpper false (name.ToCharArray() |> Array.toList)


let shoutGreeting greeting name = 
    let (greeting:string) = greeting name
    greeting.ToUpper()

let greet' greeting name = 
    match isUpper name with
    |true ->  shoutGreeting greeting name
    |false -> greeting name

let greet name =
    let greeting = sprintf "Hello, %s"

    match name with
    |"" -> greet' greeting "my friend"
    |_ -> greet' greeting name 


[<Fact>]
let ``Hello name`` () =
    Assert.Equal("Hello, Gabriel",greet("Gabriel"))
    Assert.Equal("Hello, my friend",greet(""))

[<Fact>]
let ``Shout greeting`` () =
    Assert.Equal("HELLO, GABRIEL",greet("GABRIEL"))