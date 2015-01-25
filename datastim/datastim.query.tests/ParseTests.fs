namespace datastim.query.tests

module ParseTests = 
    open datastim.query
    open NUnit.Framework
    open FParsec
    open FsUnit
    open QueryModule
    
    let test result expected = 
        match result with
        | Success(result, _, _) -> result |> should equal expected
        | Failure(message, _, _) -> Assert.Fail(message)
    
    [<TestFixture>]
    type ``Parse tests``() = 
        
        [<Test>]
        member x.``Parse expand property``() = 
            let query = "Feature expand AssignedTo"
            let result = parse query
            test result (Expanded("Feature", [ Property("AssignedTo") ]))
        
        [<Test>]
        member x.``Parse expand multiple properties``() = 
            let query = "Feature expand AssignedTo,RequestedBy"
            let result = parse query
            test result (Expanded("Feature", 
                                  [ Property("AssignedTo")
                                    Property("RequestedBy") ]))
        
        [<Test>]
        member x.``Parse expand sub-property``() = 
            let query = "Group expand Members/Name"
            let result = parse query
            test result (Expanded("Group", [ Path("Members", Property("Name"))]))
