namespace JsonParser.UnitTests

open NUnit.Framework
open JsonParser.Parser
open JsonParser
open Either

[<TestFixture>]
module ParserTests = 
    [<Test>]
    let parse_simple_number_should_return_a_number_ast_node () = 
        let text = "123455"
        let ast = parse text
        let expected = Number(text)

        Assert.AreEqual(expected, ast)

    [<Test>]
    let parse_simple_null_keyword_should_return_a_null_ast_node () = 
        let text = "null"
        let ast = parse text
        let expected = Null

        Assert.AreEqual(expected, ast )

    [<Test>]
    let parse_simple_truel_keyword_should_return_a_null_ast_node () = 
        let text = "true"
        let ast = parse text
        let expected = Boolean(true)

        Assert.AreEqual(expected, ast)

    [<Test>]
    let parse_simple_false_keyword_should_return_a_null_ast_node () = 
        let text = "false"
        let ast = parse text
        let expected = Boolean(false)

        Assert.AreEqual(expected, ast)

    [<Test>]
    let parse_array_with_no_elements_should_return_empty_array () = 
        let text = "[]"
        let ast = parse text
        let expected = Array([])

        Assert.AreEqual(expected, ast)
       
    [<Test>]
    let parse_array_with_single_element_should_return_correct_result () = 
        let text = "[1]"
        let ast = parse text
        let expected = Array([Number("1");])

        Assert.AreEqual(expected, ast)         

    [<Test>]
    let parse_array_with_2_elements_should_return_correct_result () = 
        let text = "[1,2]"
        let ast = parse text
        let expected = Array([Number("1");Number("2");])

        Assert.AreEqual(expected, ast)   
     
    [<Test>]
    let parse_array_with_2_element_nested_array_should_return_correct_result () = 
        let text = "[[1,2]]"
        let ast = parse text
        let expected =  Array([Array([Number("1");Number("2");]);])

        Assert.AreEqual(expected, ast)   
           
    [<Test>]
    let parse_array_with_2_2_element_nested_array_should_return_correct_result () = 
        let text = "[[1,2],[3,4]]"
        let ast = parse text
        let expected =  Array([Array([Number("1");Number("2");]);Array([Number("3");Number("4");]);])

        Assert.AreEqual(expected, ast)   

    [<Test>]
    let parse_object_with_simple_string_property_with_number_value_should_return_correct_result () = 
        let text =  @"{ ""Age"" : 21 }"
        let ast = parse text
        let expected =  Obj([("Age", Number("21"));])

        Assert.AreEqual(expected, ast)  

    [<Test>]
    let parse_object_with_simple_string_property_with_array_as_the_value_should_return_correct_result () = 
        let text =  @"{ ""Age"" : [1,2] }"
        let ast = parse text
        let expected =  Obj([("Age", Array([Number("1");Number("2");]));])

        Assert.AreEqual(expected, ast)  