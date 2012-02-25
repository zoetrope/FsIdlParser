module ParserTest.StructParserTest

open FParsec.Primitives
open FParsec.CharParsers
open FParsec.Error

open NaturalSpec

open idl.parser
open idl.ast
        
let runTest f x =
    f |> run <| x |> extractExprs

[<Scenario>]
let ``Structをパースする`` ()=
    Given "struct Test { long abc; string hoge;};"
        |> When runTest structType
        |> It should equal (Struct("Test",[(Member (Primitive "long",[SimpleDec "abc"]));
                                           (Member (String (Literal(["0"])),[SimpleDec "hoge"]))]))
        |> Verify

[<Scenario>]
let ``型のメンバーをパースする`` ()=
    Given "struct ManagerProfile{NVList properties;};"
        |> When runTest structType
        // TODO
        |> Verify

[<Scenario>]
let ``short型のメンバーをパースする`` ()=
    Given "short hoge;"
        |> When runTest memberDec
        |> It should equal (Member (Primitive "short",[SimpleDec "hoge"]))
        |> Verify

[<Scenario>]
let ``long型のメンバーをパースする`` ()=
    Given "long  hoge;"
        |> When runTest memberDec
        |> It should equal (Member (Primitive "long",[SimpleDec "hoge"]))
        |> Verify
    
[<Scenario>]
let ``long longに引っかからないように`` ()=
    Given "long longHoge;"
        |> When runTest memberDec
        |> It should equal (Member (Primitive "long",[SimpleDec "longHoge"]))
        |> Verify
    
[<Scenario>]
let ``unsigned long型のメンバーをパースする`` ()=
    Given "unsigned long hoge ;"
        |> When runTest memberDec
        |> It should equal (Member (Primitive "unsigned long",[SimpleDec "hoge"]))
        |> Verify

[<Scenario>]
let ``string型のメンバーをパースする`` ()=
    Given "string hoge;"
        |> When runTest memberDec
        |> It should equal (Member (String (Literal(["0"])),[SimpleDec "hoge"]))
        |> Verify
                
[<Scenario>]
let ``stringっぽい型のメンバーをパースする`` ()=
    Given "stringBuilder hoge;"
        |> When runTest memberDec
        |> It should equal (Member (ScopedType ["stringBuilder"],[SimpleDec "hoge"]))
        |> Verify

[<Scenario>]
let ``floatっぽい型のメンバーをパースする`` ()=
    Given "floathoge hoge;"
        |> When runTest memberDec
        |> It should equal (Member (ScopedType ["stringBuilder"],[SimpleDec "hoge"]))
        |> Verify
                
[<Scenario>]
let ``型と変数名の間のスペースがない場合`` ()=
    Given "stringhoge;"
        |> When runTest memberDec
        //TODO
        |> Verify

[<Scenario>]
let ``独自型のメンバーをパースする`` ()=
    Given "Test hoge;"
        |> When runTest memberDec
        |> It should equal (Member (ScopedType ["Test"],[SimpleDec "hoge"]))
        |> Verify

[<Scenario>]
let ``複数のメンバーをパースする`` ()=
    Given "octet  hoge ,  fuga ;"
        |> When runTest memberDec
        |> It should equal (Member (Primitive "octet",[SimpleDec "hoge"; SimpleDec "fuga"]))
        |> Verify
                