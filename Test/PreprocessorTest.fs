module ParserTest.PreprocessorTest

open NaturalSpec

open idl.parser
open idl.ast
open idl.preprocessor
        
[<Scenario>]
let ``1行コメント`` ()=
    Given "//hogehoge" 
        |> When deleteComment
        |> It should equal ""
        |> Verify

[<Scenario>]
let ``途中から1行コメント`` ()=
    Given "testtest //hogehoge" 
        |> When deleteComment
        |> It should equal "testtest "
        |> Verify

[<Scenario>]
let ``ブロックコメント1行`` ()=
    Given "/*hogehoge*/" 
        |> When deleteComment
        |> It should equal ""
        |> Verify

[<Scenario>]
let ``ブロックコメント途中`` ()=
    Given "test /*hogehoge*/ hoge" 
        |> When deleteComment
        |> It should equal "test  hoge"
        |> Verify
                
[<Scenario>]
let ``1行に複数のブロックコメント`` ()=
    Given "test /*hoge*/ hoge /*hoge*/ fuga" 
        |> When deleteComment
        |> It should equal "test  hoge  fuga"
        |> Verify
                
[<Scenario>]
let ``ブロックコメントの中にラインコメント`` ()=
    Given "test /* // hoge*/ fuga" 
        |> When deleteComment
        |> It should equal "test  fuga"
        |> Verify

[<Scenario>]
let ``複数行コメント`` ()=
    Given "/*hoge\r\nfuga\r\nbar*/" 
        |> When deleteComment
        |> It should equal ""
        |> Verify


