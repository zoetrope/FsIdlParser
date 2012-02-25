module ParserTest.BasicParserTeset

open FParsec.Primitives
open FParsec.CharParsers
open FParsec.Error

open NaturalSpec

open idl.parser
open idl.ast

let runTest f x =
    f |> run <| x |> extractExprs

[<Scenario>]
let ``空のmoduleをパースする`` ()=
    Given "module hoge { }"
        |> When runTest moduleDcl
        |> It should equal (Module("hoge",[]))
        |> Verify

[<Scenario>]
let ``空のmoduleをパースする2`` ()=
    Given "module hoge { };"
        |> When runTest specification
        |> It should equal ([Module("hoge",[])])
        |> Verify

[<Scenario>]
let ``空のmoduleをパースする3`` ()=
    Given "module hoge { struct Time { long sec; long nsec;}; };"
        |> When runTest specification
        |> It should equal ([Module("hoge",[Struct("Time",[Member (Primitive "long",[SimpleDec "sec"]);Member (Primitive "long",[SimpleDec "nsec"])])])])
        |> Verify

[<Scenario>]
let ``通常のdeclaratorをパースする`` ()=
    Given "hoge"
        |> When runTest declarator
        |> It should equal (SimpleDec "hoge")
        |> Verify

[<Scenario>]
let ``配列形式をパースする`` ()=
    Given "hoge[2]"
        |> When runTest declarator
        |> It should equal (ArrayDec("hoge",[(Literal(["2"]))]))
        |> Verify


[<Scenario>]
let ``stringをパースする`` ()=
    Given "string"
        |> When runTest stringType
        |> It should equal (String (Literal(["0"])))
        |> Verify

[<Scenario>]
let ``string<1>をパースする`` ()=
    Given "string<1>"
        |> When runTest stringType
        |> It should equal (String (Literal(["1"])))
        |> Verify
                
[<Scenario>]
let ``wstringをパースする`` ()=
    Given "wstring"
        |> When runTest wideStringType
        |> It should equal (WString (Literal(["0"])))
        |> Verify

[<Scenario>]
let ``wstring<1>をパースする`` ()=
    Given "wstring<1>"
        |> When runTest wideStringType
        |> It should equal (WString (Literal(["1"])))
        |> Verify
          

[<Scenario>]
let ``sequenceをパースする`` ()=
    Given "sequence<long>"
        |> When runTest sequenceType
        |> It should equal (Sequence (Primitive "long", (Literal(["0"]))))
        |> Verify
      
[<Scenario>]
let ``enumをパースする`` ()=
    Given "enum hoge { foo, bar, hoga , homu }"
        |> When runTest enumType
        |> It should equal (Enum ("hoge",["foo"; "bar"; "hoga"; "homu"]))
        |> Verify
        
[<Scenario>]
let ``unionをパースする`` ()=
    Given "  union Numeric switch (NumericType){case SHORT_TYPE:  short short_value;case LONG_TYPE: long long_value;};"
        |> When runTest unionType
        |> It should equal (Enum ("hoge",["foo"; "bar"; "hoga"; "homu"]))
        |> Verify

      
      
[<Scenario>]
let ``sequenceのtypedefをパースする`` ()=
    Given "typedef sequence<long> LongSeq;"
        |> When runTest typedefDcl
        |> It should equal (Typedef (Sequence (Primitive "long",(Literal(["0"]))),SimpleDec "LongSeq"))
        |> Verify

[<Scenario>]
let ``ScopedTypeのtypedefをパースする`` ()=
    Given "typedef SDOPackage::NVList NVList;"
        |> When runTest typedefDcl
        |> It should equal (Typedef (ScopedType ["SDOPackage";"NVList"],SimpleDec "NVList"))
        |> Verify
                
 