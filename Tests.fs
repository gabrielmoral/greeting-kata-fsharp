module Tests

open System
open Xunit

let greet name =
    let name' = match name with
                |"" -> "my friend"
                | _ -> name

    sprintf "Hello, %s" name'

[<Fact>]
let ``Hello name`` () =
    Assert.Equal("Hello, Gabriel",greet("Gabriel"))
    Assert.Equal("Hello, my friend",greet(""))