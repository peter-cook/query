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
        member x.``Parse only entity``() = 
            let query = "Feature"
            let result = parse query
            test result (Query("Feature", []))
        
        [<Test>]
        member x.``Parse where property equals a value``() = 
            let query = "Member where Name = 'Mark'"
            let result = parse query
            test result (Query("Member", [ Where(Condition(Property("Name"), Equals, String("Mark"))) ]))

        [<Test>]
        member x.``Parse where property equals another value``() = 
            let query = "Member where Name = 'John'"
            let result = parse query
            test result (Query("Member", [ Where(Condition(Property("Name"), Equals, String("John"))) ]))
        
        [<Test>]
        member x.``Parse where sub-property equals a value``() = 
            let query = "Member where AssignedTo/Name = 'Mark'"
            let result = parse query
            test result 
                (Query("Member", [ Where(Condition(Path("AssignedTo", Property("Name")), Equals, String("Mark"))) ]))
        
        [<Test>]
        member x.``Parse expand property and a property equals a value``() = 
            let query = "Feature expand AssignedTo where Name = 'Mark'"
            let result = parse query
            test result (Query("Feature", 
                               [ Expand([ Property("AssignedTo") ])
                                 Where(Condition(Property("Name"), Equals, String("Mark"))) ]))
        
        [<Test>]
        member x.``Parse expand property``() = 
            let query = "Feature expand AssignedTo"
            let result = parse query
            test result (Query("Feature", [ Expand([ Property("AssignedTo") ]) ]))
        
        [<Test>]
        member x.``Parse expand multiple properties``() = 
            let query = "Feature expand AssignedTo,RequestedBy"
            let result = parse query
            test result (Query("Feature", 
                               [ Expand([ Property("AssignedTo")
                                          Property("RequestedBy") ]) ]))
        
        [<Test>]
        member x.``Parse expand multiple properties (with space after comma)``() = 
            let query = "Feature expand AssignedTo, RequestedBy"
            let result = parse query
            test result (Query("Feature", 
                               [ Expand([ Property("AssignedTo")
                                          Property("RequestedBy") ]) ]))
        
        [<Test>]
        member x.``Parse expand multiple properties (one a sub-property)``() = 
            let query = "Feature expand AssignedTo,RequestedBy/Groups"
            let result = parse query
            test result (Query("Feature", 
                               [ Expand([ Property("AssignedTo")
                                          Path("RequestedBy", Property("Groups")) ]) ]))
        
        [<Test>]
        member x.``Parse expand sub-property``() = 
            let query = "Group expand Members/Groups"
            let result = parse query
            test result (Query("Group", [ Expand([ Path("Members", Property("Groups")) ]) ]))
        
        [<Test>]
        member x.``Parse expand sub-sub-property``() = 
            let query = "Group expand Members/Groups/Members"
            let result = parse query
            test result (Query("Group", [ Expand([ Path("Members", Path("Groups", Property("Members"))) ]) ]))
