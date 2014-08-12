namespace JsonParser.UnitTests
open System
open NUnit.Framework
open JsonParser.Scanner
open JsonParser

[<TestFixture>]
module ScannerTests = 

    [<Test>]
    let tokenize_string_of_characters_should_have_a_matching_token () = 
        let text = @"""Hello World"""
        let token = tokenizer text
        match token with
            | Left(v) -> Assert.Fail(v)
            | Right(v) -> Assert.AreEqual(String("Hello World"), List.head v)

    [<Test>]
    let tokenize_string_of_numbers_should_have_a_matching_token () = 
        let text = "123455"
        let token = tokenizer text

        match token with
            | Left(v) -> Assert.Fail(v)
            | Right(v) -> Assert.AreEqual(Number(text), List.head v)

    [<Test>]
    let tokenize_string_of_numbers_should_only_return_one_element_in_the_list () = 
        let text = "123455"
        let token = tokenizer text

        match token with
            | Left(v) -> Assert.Fail(v)
            | Right(v) -> Assert.AreEqual(1, List.length v)

    [<Test>]
    let tokenize_array_of_numbers_should_return_array_of_correct_elements () = 
        let text = "[1,2,3,4,5,66]"
        let tokens = tokenizer text
        let list = [OpenBracket;Number("1");Comma;Number("2");Comma;Number("3");Comma;Number("4");Comma;Number("5");Comma;Number("66");CloseBracket]

        match tokens with
            | Left(v) -> Assert.Fail(v)
            | Right(v) -> Assert.AreEqual(list, v)

    [<Test>]
    let tokenize_null_should_have_a_matching_token () = 
        let text = "null"
        let token = tokenizer text

        match token with
            | Left(v) -> Assert.Fail(v)
            | Right(v) -> Assert.AreEqual(Null, List.head v)

    [<Test>]
    let tokenize_null_should_only_return_one_element_in_the_list () = 
        let text = "null"
        let token = tokenizer text

        match token with
            | Left(v) -> Assert.Fail(v)
            | Right(v) -> Assert.AreEqual(1, List.length v)    

    
    [<Test>]
    let tokenize_true_should_have_a_matching_token () = 
        let text = "true"
        let token = tokenizer text

        match token with
            | Left(v) -> Assert.Fail(v)
            | Right(v) -> Assert.AreEqual(True, List.head v)

    [<Test>]
    let tokenize_true_should_only_return_one_element_in_the_list () = 
        let text = "true"
        let token = tokenizer text

        match token with
            | Left(v) -> Assert.Fail(v)
            | Right(v) -> Assert.AreEqual(1, List.length v)  

    [<Test>]
    let tokenize_false_should_have_a_matching_token () = 
        let text = "false"
        let token = tokenizer text

        match token with
            | Left(v) -> Assert.Fail(v)
            | Right(v) -> Assert.AreEqual(False, List.head v)

    [<Test>]
    let tokenize_false_should_only_return_one_element_in_the_list () = 
        let text = "true"
        let token = tokenizer text

        match token with
            | Left(v) -> Assert.Fail(v)
            | Right(v) -> Assert.AreEqual(1, List.length v)  