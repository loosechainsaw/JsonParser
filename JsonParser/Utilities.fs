namespace JsonParser

open System

module Utilities = 
    let (|MatchColon|_|) c = 
        if c = ':' then
            Some(c)
        else
            None

    let (|MatchComma|_|) c = 
        if c = ',' then
            Some(c)
        else
            None

    let (|MatchDoubleQuotes|_|) c = 
        if c = '"' then
            Some(c)
        else
            None

    let (|MatchPlus|_|) c = 
        if c = '+' then
            Some(c)
        else
            None

    let (|MatchMinus|_|) c = 
        if c = '-' then
            Some(c)
        else
            None

    let (|MatchOpenParen|_|) c = 
        if c = '(' then
            Some(c)
        else
            None

    let (|MatchCloseBrace|_|) c = 
        if c = '}' then
            Some(c)
        else
            None

    let (|MatchOpenBrace|_|) c = 
        if c = '{' then
            Some(c)
        else
            None

    let (|MatchCloseParen|_|) c = 
        if c = ')' then
            Some(c)
        else
            None

    let (|MatchOpenBracket|_|) c = 
        if c = '[' then
            Some(c)
        else
            None

    let (|MatchCloseBracket|_|) c = 
        if c = ']' then
            Some(c)
        else
            None

    let (|MatchEscapedDoubleQuotes|_|) c = 
        if c = '\"' then
            Some(c)
        else
            None

    let (|MatchWhitespace|_|) c =
        if Char.IsWhiteSpace(c) then
            Some("")
        else
            None

    let (|MatchDigit|_|) c = 
        if Char.IsDigit(c) then
            Some(c)
        else
            None