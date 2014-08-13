namespace JsonParser

open JsonParser
open JsonParser.Scanner
open System

module Parser = 
    
    type AbstractSyntaxTree = 
        | String of string
        | Number of string
        | Boolean of bool
        | Null
        | Array of AbstractSyntaxTree list
        | Obj of (String * AbstractSyntaxTree) list


    type TokenStream = Token list

    type ParseExpression = Either<string,(TokenStream * AbstractSyntaxTree)>

    type ArrayBody = Either<string,(TokenStream *  AbstractSyntaxTree list)>

    type ObjectBody = Either<string,(TokenStream *  (String * AbstractSyntaxTree) list)>

    let rec parse_array (input:TokenStream) : ParseExpression = 
        match input with
            | OpenBracket :: t -> 

                parse_array_body t >>= (fun t -> 
                                            let unparsed, body = t

                                            match unparsed with
                                                | [] -> Left("Unexpected end of input")
                                                | CloseBracket :: rest ->
                                                    Right((rest, Array(body)))
                                                | _ -> Left("Error")

                                        )

            | _ -> Left("Invalid token")

    and parse_array_body (input:TokenStream) : ArrayBody =
        match input with
            | CloseBracket :: t -> Right((input, []))
            | Comma :: CloseBracket :: t -> Left("Invalid format")
            | t -> 
                parse_impl t >>= (fun t -> 
                                    let unparsed, element = t

                                    match unparsed with
                                        | CloseBracket :: t -> Right(unparsed, element :: [])
                                        | Comma :: rest ->
                                            (parse_array_body rest) >>= (fun t -> 
                                                                            let unprocessed, otherelement = t
                                                                            Right((unprocessed, element :: otherelement))
                                                                        )
                                        | _ -> Left("Comma expected")

                                )
            | _ -> Left("Invalid token")
               
    and parse_object (input:TokenStream) : ParseExpression = 
        match input with
            | OpenBrace :: t -> 
                parse_object_body t >>= (fun t -> 
                                            let unparsed, body = t

                                            match unparsed with
                                                | [] -> Left("Unexpected end of input")
                                                | CloseBrace :: rest ->
                                                    Right((rest, Obj(body)))
                                                | _ -> Left("Error")

                                        )

            | _ -> Left("Invalid token")

    and parse_object_body (input:TokenStream) : ObjectBody =
        match input with
            | CloseBrace :: t -> Right((input, []))
            | Comma :: CloseBrace :: t | Colon :: Comma :: t -> Left("Invalid format")
            | Token.String(label) :: Colon :: t -> 
                parse_impl t >>= (fun t -> 
                                        let unparsed, element = t

                                        match unparsed with
                                            | CloseBrace :: t -> Right(unparsed, (label, element) :: [])
                                            | Comma :: rest ->
                                                parse_object_body rest >>= (fun t -> 
                                                                                let unprocessed, otherelement = t
                                                                                Right((unprocessed, (label, element) :: otherelement))
                                                                            )

                                            | _ -> Left("Comma expected")
                                )
            | _ -> Left("Invalid token")

    and parse_primative (input: TokenStream) : ParseExpression =
        
        match input with
            | Token.String(value) :: t -> Right ((t, String(value)))
            | Token.Null :: t -> Right ((t, Null))
            | Token.Number(value) :: t -> Right ((t, Number(value)))
            | Token.False :: t -> Right ((t, Boolean(false)))
            | Token.True :: t -> Right ((t, Boolean(true)))
            | _ -> Left("Unexpected primative")
    
    and parse_impl (tokens:TokenStream) : ParseExpression = 
        match tokens with
            | [] -> Left("Empty")
            | OpenBracket :: t -> parse_array tokens  
            | OpenBrace :: t -> parse_object tokens  
            | h :: t ->  parse_primative tokens

    and parse (text : string) : Either<string,AbstractSyntaxTree> = 
        let tokens = Scanner.tokenizer text
        
        tokens >>= parse_impl >>= 
            (fun t -> 
                 let rest, result = t
                 match rest with
                    | [] -> Right(result)
                    | _ -> Left("Unconsumed tokens")
            )

               

                