module ParserTest.InterfaceParserTest

open FParsec.Primitives
open FParsec.CharParsers
open FParsec.Error

open NaturalSpec

open idl.parser
open idl.ast

let runTest f x =
    f |> run <| x |> extractExprs

[<Scenario>]
let ``もっとも基本的なInterfaceをパースする`` ()=
    Given "interface Test{};"
        |> When runTest interfaceType
        |> It should equal (Interface("Test",[],[], null))
        |> Verify

[<Scenario>]
let ``継承ありのInterfaceをパースする`` ()=
    Given "interface Test : Hoge {};"
        |> When runTest interfaceDcl
        |> It should equal (Interface("Test",[ScopedType ["Hoge"]],[], null))
        |> Verify
                
[<Scenario>]
let ``複数の継承ありのInterfaceをパースする`` ()=
    Given "interface Test : Hoge, Fuga{};"
        |> When runTest interfaceType
        |> It should equal (Interface("Test",[ScopedType ["Hoge"];ScopedType ["Fuga"]],[], null))
        |> Verify
                
[<Scenario>]
let ``前置方式の場合`` ()=
    Given "interface Test;"
        |> When runTest interfaceType
        |> It should equal (ForwardInterface("Test", null))
        |> Verify


[<Scenario>]
let ``Managerをパースする`` ()=
    Given "interface Manager{RTC::ReturnCode_t fork();};"
        |> When runTest interfaceDcl
        |> It should equal (Interface("Manager",[],[Operation ("fork",[],[],[],ScopedType ["RTC"; "ReturnCode_t"],null)],null))
        |> Verify

                  

[<Scenario>]
let ``戻り値と引数なしのOperationをパースする`` ()=
    Given "void Test();"
        |> When runTest opDcl
        |> It should equal (Operation("Test",[],[],[],Void,null))
        |> Verify

[<Scenario>]
let ``戻り値ありのOperationをパースする`` ()=
    Given "char Test();"
        |> When runTest opDcl
        |> It should equal (Operation("Test",[],[],[],Primitive "char",null))
        |> Verify
                
[<Scenario>]
let ``引数ありのOperationをパースする`` ()=
    Given "void Test(in float val);"
        |> When runTest opDcl
        |> It should equal (Operation("Test",[Parameter ("in",Primitive "float",SimpleDec "val")],[],[],Void,null))
        |> Verify

[<Scenario>]
let ``複数引数のOperationをパースする`` ()=
    Given "void Test(in float val1 , inout double val2,  out  string   val3  );"
        |> When runTest opDcl
        |> It should equal (Operation("Test",[Parameter ("in",Primitive "float",SimpleDec "val1");
                                              Parameter ("inout",Primitive "double",SimpleDec "val2");
                                              Parameter ("out",String (Literal(["0"])),SimpleDec "val3")],[],[],Void,null))
        |> Verify
    
[<Scenario>]
let ``wstringの引数をパースする`` ()=
    Given "void Test(in wstring val);"
        |> When runTest opDcl
        |> It should equal (Operation("Test",[Parameter ("in",WString (Literal(["0"])),SimpleDec "val")],[],[],Void,null))
        |> Verify
                
                    
[<Scenario>]
let ``longっぽい型の戻り値をパースする`` ()=
    Given "longHoge hoge();"
        |> When runTest opDcl
        |> It should equal (Operation("hoge",[],[],[],ScopedType ["longHoge"],null))
        |> Verify

[<Scenario>]
let ``octetっぽい引数をパースする`` ()=
    Given "void hoge(in octetoctet val);"
        |> When runTest opDcl
        |> It should equal (Operation("hoge",[Parameter ("in",ScopedType ["octetoctet"],SimpleDec "val")],[],[],Void,null))
        |> Verify
                
[<Scenario>]
let ``引数の型と名前の間にスペースがない場合`` ()=
    Given "void Test(inlongval);"
        |> When runTest opDcl
        //TODO: 失敗チェック
        |> Verify
 
[<Scenario>]
let ``戻り値と名前の間にスペースがない場合`` ()=
    Given "voidTest();"
        |> When runTest opDcl
        //TODO: 失敗チェック
        |> Verify

[<Scenario>]
let ``in引数のチェック`` ()=
    Given "in long hoge"
        |> When runTest paramDcl
        |> It should equal (Parameter("in", Primitive "long", SimpleDec "hoge"))
        |> Verify
                
[<Scenario>]
let ``out引数のチェック`` ()=
    Given "out unsigned long hoge"
        |> When runTest paramDcl
        |> It should equal (Parameter("out", Primitive "unsigned long", SimpleDec "hoge"))
        |> Verify
                
[<Scenario>]
let ``inout引数のチェック`` ()=
    Given "inout Test hoge"
        |> When runTest paramDcl
        |> It should equal (Parameter("inout", ScopedType ["Test"], SimpleDec "hoge"))
        |> Verify

