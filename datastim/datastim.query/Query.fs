namespace datastim.query

open Deedle
open FParsec

module QueryModule = 
    
    type EntityPart = string
    
    type PropertyPath = 
        | Property of string 
        | Path of string * PropertyPath

    type ExpandPart = list<PropertyPath>

    type Op = 
        | Equals

    type Literal = 
        | String of string

    type WherePart = 
        | Condition of PropertyPath * Op * Literal

    type QueryPart = 
        | Expand of ExpandPart
        | Where of WherePart

    type Query = 
        | Query of EntityPart * list<QueryPart>
    
    let parse (query : string) = 
        
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

        let pPropertyPaths = sepBy pPropertyPath (pstring "," .>> spaces)

        let pExpand = pstringCI "expand" .>> spaces

        let pExpand = pExpand >>. pPropertyPaths |>> (fun x -> Expand(x))

        let pWhere = pstringCI "where" .>> spaces

        let pEquals = pstring "=" .>> spaces |>> (fun _ -> Equals)

        let stringLiteral =
            let normalCharSnippet = manySatisfy (fun c -> c <> '\\' && c <> ''')
            let escapedChar = pstring "\\" >>. (anyOf "\\nrt'" |>> function
                                                            | 'n' -> "\n"
                                                            | 'r' -> "\r"
                                                            | 't' -> "\t"
                                                            | c   -> string c)
            between (pstring "'") (pstring "'")
                    (stringsSepBy normalCharSnippet escapedChar)

        let pLiteral = stringLiteral

        let pWhereClause = pipe3 pPropertyPath pEquals pLiteral (fun x y z -> Condition(x,y,String(z)))

        let pWhere = pWhere >>. pWhereClause |>> (fun x -> Where(x))

        let pQueryParts = many (choice [ pExpand; pWhere ])

        let parser = pipe2 pEntity pQueryParts (fun e qp -> Query(e,qp))

        run parser query
    
    let evaluate query = 1
