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

    type ParseExpression = (TokenStream * AbstractSyntaxTree)

    type ArrayBody = (TokenStream * AbstractSyntaxTree list)

    type ObjectBody = (TokenStream * (String * AbstractSyntaxTree) list)

    let rec parse_array (input:TokenStream) : ParseExpression = 
        match input with
            | OpenBracket :: t -> 
                let unparsed, body = parse_array_body t

                match unparsed with
                    | [] -> failwith "Unexpected end of input"
                    | CloseBracket :: rest ->
                        (rest, Array(body))
                    | _ -> failwith "Error"

            | _ -> failwith "Invalid token"

    and parse_array_body (input:TokenStream) : ArrayBody =
        match input with
            | CloseBracket :: t -> (input, [])
            | Comma :: CloseBracket :: t -> failwith "Invalid format"
            | t -> 
                let unparsed, element = parse_impl t

                match unparsed with
                    | CloseBracket :: t -> unparsed, element :: []
                    | Comma :: rest ->
                        let unprocessed, otherelement = parse_array_body rest
                        (unprocessed, element :: otherelement)
                    | _ -> failwith "Comma expected"

               
    and parse_object (input:TokenStream) : ParseExpression = 
        match input with
            | OpenBrace :: t -> 
                let unparsed, body = parse_object_body t

                match unparsed with
                    | [] -> failwith "Unexpected end of input"
                    | CloseBrace :: rest ->
                        (rest, Obj(body))
                    | _ -> failwith "Error"

            | _ -> failwith "Invalid token"

    and parse_object_body (input:TokenStream) : ObjectBody =
        match input with
            | CloseBrace :: t -> (input, [])
            | Comma :: CloseBrace :: t | Colon :: Comma :: t -> failwith "Invalid format"
            | Token.String(label) :: Colon :: t -> 
                let unparsed, element = parse_impl t

                match unparsed with
                    | CloseBrace :: t -> unparsed, (label, element) :: []
                    | Comma :: rest ->
                        let unprocessed, otherelement = parse_object_body rest
                        (unprocessed, (label, element) :: otherelement)
                    | _ -> failwith "Comma expected"

    and parse_primative (input: TokenStream) : ParseExpression =
        
        match input with
            | Token.String(value) :: t -> (t, String(value))
            | Token.Null :: t -> (t, Null)
            | Token.Number(value) :: t -> (t, Number(value))
            | Token.False :: t -> (t, Boolean(false))
            | Token.True :: t -> (t, Boolean(true))
            | _ -> failwith "Unexpected primative"
    
    and parse_impl (tokens:TokenStream) : ParseExpression = 
        match tokens with
            | [] -> failwith "Empty"
            | OpenBracket :: t -> parse_array tokens  
            | OpenBrace :: t -> parse_object tokens  
            | h :: t ->  parse_primative tokens

    and parse (text : string) : AbstractSyntaxTree = 
        let tokens = Scanner.tokenizer text
        
        match tokens with
            | Left(v) -> failwith v
            | Right(v) -> 
                let rest, result = parse_impl v

                match rest with
                    | [] -> result
                    | _ -> failwith "Unconsumed tokens"