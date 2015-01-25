namespace datastim.query

module EvaluateModule = 
    open FParsec
    open System.Collections.Generic
    open System
    open ParserModule
    open Deedle
    
    type SourceDataSet() = 
        let entities = new Dictionary<string, string * seq<Object>>()
        let links = new Dictionary<string * string, seq<Object>>()
        member x.GetEntities(entity) = entities.[entity]
        member x.AddEntities(entity, key, data) = entities.Add(entity, (key, data))
        member x.AddLinks(entity1, entity2, data) = links.Add((entity1, entity2), data)
    
    let evaluate (sourceDataSet : SourceDataSet) (query : string) = 
        let parsedQuery = parse query
        match parsedQuery with
        | (Query(entity, queryParts)) -> 
            let key, data = sourceDataSet.GetEntities(entity)
            let entityFrame = Frame.ofRecords (data)
            entityFrame.ToArray2D()
