module idl.ast

open System
open FParsec.Primitives
open FParsec.CharParsers
open FParsec.Error

type ConstExpr = 
    | Xor               of ConstExpr * ConstExpr
    | Or                of ConstExpr * ConstExpr
    | And               of ConstExpr * ConstExpr
    | Shift             of ConstExpr * ConstExpr
    | Add               of ConstExpr * ConstExpr
    | Minus             of ConstExpr * ConstExpr
    | Multi             of ConstExpr * ConstExpr
    | Divide            of ConstExpr * ConstExpr
    | Rest              of ConstExpr * ConstExpr
    | Unary             of string * ConstExpr
    | Literal           of string list

and Definition =
    | Sequence          of Definition * ConstExpr
    | String            of ConstExpr
    | WString           of ConstExpr
    | Primitive         of string
    | Void
    | ScopedType        of string list

    // 構造体              名前, メンバー
    | Struct            of string * (Definition list)
    // 
    | Union             of string * Definition * (Definition list)
    | Case              of (ConstExpr list) * Definition 
    | Element           of Definition * Definition
    // 


    | Enum              of string * (string list)
    // ネームスペース       
    | Module            of string * (Definition list)
    // typedef             型, 新しい型
    | Typedef           of Definition * Definition
    // 定数                型, 名前, 値
    | Const             of string * string * string
    // 前置インタフェース  名前, 属性
    | ForwardInterface  of string * string

    // インタフェース      名前, 継承元, メンバー, 属性
    | Interface         of string * (Definition list) * (Definition list) * string
    // 例外                名前, メンバー
    | ExceptionType     of string * (Definition list)
 
    | SimpleDec         of string
    | ArrayDec          of string * (ConstExpr list)
    | AggregateDec      of Definition * (Definition list)
    
    // プロパティ          型, 名前
    | Member            of Definition * (Definition list)
    // メソッド            名前, 引数, 例外, コンテキスト, 戻り値, 属性
    | Operation         of string * (Definition list) * (Definition list) * (string list) * Definition * string
    // 引数                属性, 型, 名前
    | Parameter         of string * Definition * Definition
    // 属性                型, 
    | Attribute         of string * (Definition list)
