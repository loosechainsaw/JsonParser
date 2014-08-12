
namespace JsonParser

open System

module Scanner = 

    open Utilities

    type Token = 
        | String of string
        | Number of string
        | Null
        | True
        | False
        | OpenBracket
        | CloseBracket
        | OpenParen
        | CloseParen
        | OpenBrace
        | CloseBrace
        | Colon
        | Comma

    let rec private scan_string s (acc: string) = 
            match s with
                | [] -> 
                    Left("No futher characters to completed the string")
                | '\\' :: MatchDoubleQuotes _ :: t -> 
                    scan_string t ( acc + "\"") 
                | MatchDoubleQuotes _ :: t ->
                    Right((t , acc))
                | h  :: t ->
                    scan_string t <|  acc + string h

    let rec private scan_digit (s:char list) (acc:string) =
        match s with
            | [] -> Right((s,acc))
            | MatchComma _ :: t ->  Right((s, acc))
            | MatchColon _ :: t ->  Right((s,acc))
            | MatchWhitespace _ :: t ->  Right((s,acc))
            | MatchCloseBracket _ :: t ->  Right((s,acc))
            | MatchCloseBrace _ :: t ->  Right((s,acc))
            | MatchDigit h :: t -> scan_digit t (acc + string h)
            | _ -> Left("Unexpected digit processed")

    let private literal_map input =
        match input with
            | "null" -> Some(Null)
            | "true" -> Some(True)
            | "false" -> Some(False)
            | _ -> None

    let rec private scan_other (s:char list) (acc:string) =
        match s with
            | [] -> Right((s, literal_map acc))
            | MatchComma _ :: t -> Right((s, literal_map acc))
            | MatchColon _ :: t -> Right((s,literal_map acc))
            | MatchWhitespace _ :: t -> Right((t,literal_map acc))
            | h :: t -> scan_other t (acc + string h)
            | _ -> Left("Unexpected character found in processing token")

    let tokenizer (input:string) = 
        let rec tokenizer_impl (input:char list) (acc: Token list) =
            match input with
                | [] -> Right(acc)
                | MatchWhitespace _ :: t -> 
                    tokenizer_impl t acc
                | MatchColon _ :: t -> 
                    tokenizer_impl t ( Colon :: acc)
                | MatchComma _ :: t -> 
                    tokenizer_impl t ( Comma :: acc)
                | MatchOpenParen _ :: t -> 
                    tokenizer_impl t ( OpenParen :: acc)
                | MatchCloseParen _ :: t -> 
                    tokenizer_impl t ( CloseParen :: acc)
                | MatchCloseBracket _ :: t -> 
                    tokenizer_impl t ( CloseBracket :: acc)
                | MatchOpenBracket _ :: t -> 
                    tokenizer_impl t ( OpenBracket :: acc)
                | MatchCloseBrace _ :: t -> 
                    tokenizer_impl t ( CloseBrace :: acc)
                | MatchOpenBrace _ :: t -> 
                    tokenizer_impl t ( OpenBrace :: acc)
                | MatchDoubleQuotes _ :: t -> 
                    let result = scan_string t ""
                    result >>= (fun v ->  
                                    let (a,b) = v
                                    let result = tokenizer_impl a <| String(b) :: acc 
                                    result
                                )
                | MatchDigit h :: t -> 
                    let result = scan_digit input ""
                    result >>= (fun v ->  
                                    let (a,b) = v
                                    let result = tokenizer_impl a ( Number(b) :: acc)
                                    result
                                )
                | MatchPlus _ :: MatchDigit h :: t -> 
                    let result = scan_digit input ""
                    result >>= (fun v ->  
                                    let (a,b) = v
                                    let result = tokenizer_impl a ( Number(b) :: acc)
                                    result
                                )
                | MatchMinus _ :: MatchDigit h :: t -> 
                    let result = scan_digit input "-"
                    result >>= (fun v ->  
                                    let (a,b) = v
                                    let result = tokenizer_impl a ( Number(b) :: acc)
                                    result
                                )
                | h :: t -> 
                    let result = scan_other (h :: t) ""
                    result >>= (fun v ->  
                                    let (a,b) = v

                                    match b with
                                        | None -> Left("Unable to parse a literal")
                                        | Some(c) -> 
                                            let result = tokenizer_impl a ( c :: acc)
                                            result
                                )
        let results = tokenizer_impl (List.ofArray (input.ToCharArray())) [] 
        results |> Either.map (fun x -> List.rev x)