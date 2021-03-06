﻿namespace datastim.query.tests

open System

[<AllowNullLiteral>]
type Group(id, name) = 
    member x.Id = id
    member x.Name = name
    member x.Members : seq<Member> = null

and [<AllowNullLiteral>] Member(id, name) = 
    member x.Id = id
    member x.Name = name
    member x.Groups : seq<Group> = null

type Group_Member(group_id, member_id) = 
    member x.Group_id = group_id
    member x.Member_id = member_id

type Feature(id, summary, requestedBy_id, assignedTo_id) = 
    member x.Id = id
    member x.Summary = summary
    member x.RequestedBy : Member = null
    member x.RequestedBy_id = requestedBy_id
    member x.AssignedTo : Member = null
    member x.AssignedTo_id = assignedTo_id

module EvaluateTests = 
    open datastim.query
    open EvaluateModule
    open NUnit.Framework
    open FsUnit
    
    let groups = 
        [ new Group(1, "Administrator")
          new Group(2, "Developer")
          new Group(3, "User") ]
        |> List.toSeq
        |> Seq.cast<Object>
    
    let members = 
        [ new Member(1, "Matthew")
          new Member(2, "Mark")
          new Member(3, "Luke")
          new Member(4, "John") ]
        |> List.toSeq
        |> Seq.cast<Object>
    
    let group_member_links = 
        [ new Group_Member(1, 1)
          new Group_Member(2, 1)
          new Group_Member(3, 1)
          new Group_Member(1, 2)
          new Group_Member(2, 2)
          new Group_Member(3, 2)
          new Group_Member(2, 3)
          new Group_Member(3, 4) ]
        |> List.toSeq
        |> Seq.cast<Object>
    
    let features = 
        [ new Feature(1, "Printing", 1, 3)
          new Feature(2, "Export as PDF", 1, 2)
          new Feature(3, "Auto-formatting", 2, 0)
          new Feature(4, "Sorting", 4, 3) ]
        |> List.toSeq
        |> Seq.cast<Object>
    
    let dataSet = new SourceDataSet()
    
    dataSet.AddEntities("Feature", "Id", features)
    dataSet.AddEntities("Member", "Id", members)
    dataSet.AddEntities("Group", "Id", groups)
    dataSet.AddLinks("Group", "Member", group_member_links)
    
    [<TestFixture>]
    type ``Evaluate tests``() = 
        [<Test>]
        member x.``Evaluate expand non-collection property results in outer join``() = 
            let query = "Feature expand AssignedTo"
            let result = evaluate dataSet query 
            result |> should equal 1
