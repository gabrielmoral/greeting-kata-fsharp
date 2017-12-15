module Tests

open System
open Xunit

let greet =
    sprintf "Hello, %s"

[<Fact>]
let ``Hello name`` () =
    Assert.Equal("Hello, Gabriel",greet("Gabriel"))
