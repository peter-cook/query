namespace datastim.query

open Deedle
open FParsec

module QueryModule = 
    
    type PropertyPath = 
        | Property of string 
        | Path of string * PropertyPath

    type Entity = 
        | Expanded of string * list<PropertyPath>
        | Unexpanded of string
    
    let parse (query : string) = 
        let pExpand = pstringCI "expand" .>> spaces
        
        let pIdentifier name = 
            let isIdentifierFirstChar c = isLetter c || c = '_'
            let isIdentifierChar c = isLetter c || isDigit c || c = '_'
            many1Satisfy2L isIdentifierFirstChar isIdentifierChar name .>> spaces // skips trailing whitespace
        
        let pSlash = pstring "/"
        let pEntity = pIdentifier "entity"
        let pProperty = pIdentifier "property"
        let pChildProperty = many (pSlash >>. pProperty)

        let convertToPropertyPath (root:string) (subproperties:list<string>) : PropertyPath = 
            
            match subproperties with
            | [] ->
                Property(root)
            | head :: [] ->
                Path(root,Property(head))
            | head :: tail ->
                
                let rec buildpath x (sub:list<string>) : PropertyPath =
                    match sub with
                    | [] ->
                        Property(x)
                    | head :: tail ->
                        match tail with
                        | [] ->
                            Path(x, Property(head))
                        | h2 :: t2 ->
                            Path(x, buildpath h2 t2)

                Path(root,buildpath head tail)

        let pPropertyPath = pipe2 pProperty pChildProperty convertToPropertyPath
        let parser = pipe3 pEntity pExpand pPropertyPath (fun x y z -> Expanded(x,[z]))

        run parser query
    
    let evaluate query = 1
