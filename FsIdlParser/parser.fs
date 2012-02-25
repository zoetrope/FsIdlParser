module idl.parser

#nowarn "40" // 循環参照で警告がでないように

open System
open System.CodeDom
open FParsec.Primitives
open FParsec.CharParsers
open FParsec.Error

open idl.ast

//*******************************************************
//	Basic
//*******************************************************
let extractExprs x =
    match x with
        | Success (x, _, _) -> x
        | Failure (x,y,_) -> failwith x
        
let showAst x =
    printfn "%A" x

let ws = spaces
let str_ws s = ws >>. pstring s .>> ws

let stringLiteral =
    between (pstring "\"") (pstring "\"") (many1Chars letter)

let stringLiteralList = 
    sepBy stringLiteral (pstring ",")

let tryParse p: Parser<_,unit> =
  parse { let! _ = lookAhead p
          let! wd = p
          return wd }

let pythonIdentifier : Parser<string,unit> =
    let isAsciiIdStart    = fun c -> isAsciiLetter c || c = '_'
    let isASciiIdContinue = fun c -> isAsciiLetter c || isDigit c || c = '_'
    identifier (IdentifierOptions(isAsciiIdStart = isAsciiIdStart,
                                  isAsciiIdContinue = isASciiIdContinue,
                                  normalization = System.Text.NormalizationForm.FormKC,
                                  normalizeBeforeValidation = false,
                                  allowAllNonAsciiCharsInPreCheck = false))

let identifier =
    let isIdentifierFirstChar c = isLetter c || c = '_'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'
    (many1Satisfy2 isIdentifierFirstChar isIdentifierChar)


//*******************************************************
//	メインパーサ
//*******************************************************
let rec specification = parse{
    let! def = many definition
    do! eof
    return def}

and definition = 
    parse{
        let! t = tryParse(typeDcl)
        let! _ = str_ws ";"
        return t}
    (*
    <|> parse{
        let! c = constDcl
        let! _ = str_ws ";"
        return c}
    *)
    <|> parse{
        let! e = tryParse(exceptDcl)
        let! _ = str_ws ";"
        return e}
    <|> parse{
        let! i = tryParse(interfaceType)
        let! _ = str_ws ";"
        return i}
    <|> parse{
        let! m = tryParse(moduleDcl)
        let! _ = str_ws ";"
        return m}
    (*
    <|> parse{
        let! v = valueDcl
        let! _ = str_ws ";"
        return v}
    *)
    
//*******************************************************
//	名前空間
//*******************************************************
and moduleDcl =  parse{
    do! spaces
    let! _ = pstring "module"
    do! spaces1
    let! name = identifier
    do! spaces
    let! def = between (str_ws "{") (str_ws "}") (many definition)
    return Module(name, def)}

    
//*******************************************************
//	Interface
//*******************************************************
and interfaceType =
    tryParse(interfaceDcl)
    <|> forwardDcl
    
and interfaceDcl = parse{
    let! name, ins, attr = interfaceHeader
    let! body = between (str_ws "{") (str_ws "}") interfaceBody
    do! spaces
    return Interface(name, ins, body, attr)}

    
and forwardDcl = parse{
    do! spaces
    let! attr = pstring "abstract" <|> pstring "local" <|> parse { return null }
    let! _ = pstring "interface"
    do! spaces1
    let! name = identifier
    return ForwardInterface(name, attr)}
 
and interfaceHeader = parse{
    do! spaces
    let! attr = pstring "abstract" <|> pstring "local" <|> parse { return null }
    let! _ = pstring "interface"
    do! spaces1
    let! name = identifier
    do! spaces
    let! ins = interfaceInheritanceSpec <|> parse{ return[] }
    return name, ins, attr}

and interfaceBody = many export;

and export =
    parse{ 
        let! t = typeDcl
        let! _ = str_ws ";"
        return t}
    (*
    <|> parse{ 
        let! c = constDcl
        let! _ = str_ws ";" 
        return c}
    *)
    <|> parse{ 
        let! e = exceptDcl
        let! _ = str_ws ";"
        return e}
    (*
    <|> parse{ 
        let! a = attrDcl
        let! _ = str_ws ";" 
        return a}
    *)
    <|> parse{ 
        let! o = opDcl
        let! _ = str_ws ";"
        return o}

and interfaceInheritanceSpec = parse{
    let! _ = str_ws ":"
    let! ins = inheritanceNameList
    return ins}
        
and inheritanceNameList =
    sepBy (scopedType .>> spaces) (str_ws ",")
    
//TODO: あとで直す
//and scopedName = (many1Chars (letter <|> digit <|> (anyOf "::")))
and scopedName = parse{
    let! names = sepBy1 identifier (pstring "::")
    return names}


and scopedType = parse{
    let! name = scopedName
    return ScopedType(name)}


//*******************************************************
//	Value
//*******************************************************
//TODO: あとで
(*
and valueDcl = 
and valueForwardDcl = 
and valueBoxDcl = 
and valueAbsDcl = 
and valueDcl = 
and valueHeader = 
and valueInheritanceSpec = 
and valueName = 
and valueElement = 
and stateMember = parse{
*)

//*******************************************************
//	Init
//*******************************************************
(*
and initDcl =
and initParamDecls = 
and initParamDecl =
and initParamAttribute =
*)

//*******************************************************
//	Const
//*******************************************************
(*
and constDcl = parse{
    let! _ = str_ws "const"
    let! t = constType
    let! name = identifier
    let! _ = str_ws "="
    let! exp = constExp
    return Const(t,name,exp)}

and constType =
    integerType
    <|> charType
    <|> booleanType
    <|> floatingPtType
    <|> stringType
    <|> wideStringType
    //<|> fixedPtConstType
    <|> scopedName
    <|> octetType
 *)
   

and constExp = parse{
    let! l = literal
    return Literal([l])}
(*
and constExp = orExpr

and orExpr = 
    xorExpr 
    <|> parse{let! x1 = orExpr
              let! _ = pstring "|"
              let! x2 = xorExpr
              return Or(x1,x2)}
and xorExpr = 
    andExpr 
    <|> parse{let! x1 = xorExpr
              let! _ = pstring "^"
              let! x2 = andExpr
              return Xor(x1,x2)}
and andExpr = 
    shiftExpr 
    <|> parse{let! x1 = andExpr
              let! _ = pstring "&"
              let! x2 = shiftExpr
              return And(x1,x2)}
and shiftExpr = 
    addExpr 
    <|> parse{let! x1 = shiftExpr
              let! _ = pstring ">>"
              let! x2 = addExpr
              return Shift(x1,x2)}
    <|> parse{let! x1 = shiftExpr
              let! _ = pstring "<<"
              let! x2 = addExpr
              return Shift(x1,x2)}
and addExpr = 
    multExpr 
    <|> parse{let! x1 = addExpr
              let! _ = pstring "+"
              let! x2 = multExpr
              return Add(x1,x2)}
    <|> parse{let! x1 = addExpr
              let! _ = pstring "-"
              let! x2 = multExpr
              return Minus(x1,x2)}
and multExpr = 
    unaryExpr 
    <|> parse{let! x1 = multExpr
              let! _ = pstring "*"
              let! x2 = unaryExpr
              return Multi(x1,x2)}
    <|> parse{let! x1 = multExpr
              let! _ = pstring "/"
              let! x2 = unaryExpr
              return Divide(x1,x2)}
    <|> parse{let! x1 = multExpr
              let! _ = pstring "%"
              let! x2 = unaryExpr
              return Rest(x1,x2)}
and unaryExpr = 
    parse{let! o = unaryOperator
          let! e = primaryExpr
          return Unary(o,e)}
    <|> primaryExpr

and unaryOperator = 
    pstring "-"
    <|> pstring "+"
    <|> pstring "~"
    
and primaryExpr : Parser<ConstExpr> = 
    parse{
        let! name = scopedName
        return Literal(name)}
    <|> 
    parse{
        let! name = literal
        return Literal([name])}
    <|> between (str_ws "(") (str_ws ")") constExp
*)

//*******************************************************
//	Literal
//*******************************************************
and literal = 
    integerLiteral
    <|> stringLiteral
    //<|> wideStringLiteral
    //<|> characterLiteral
    //<|> wideCharacterLiteral
    //<|> fixedPtLiteral
    //<|> floatingPtLiteral
    <|> booleanLiteral

and booleanLiteral = 
    (str_ws "TRUE") <|> (str_ws "FALSE")
    
and positiveIntConst = constExp

and integerLiteral = many1Chars digit

//*******************************************************
//	Type
//*******************************************************
and typeDcl =
    tryParse(typedefDcl)
    <|> tryParse(structType)
    //<|> unionType
    <|> tryParse(enumType)
    //<|> nativeDcl
    //<|> constrForwardDecl
    
and typedefDcl = parse {
    do! spaces
    let! _ = pstring "typedef"
    do! spaces1
    let! t = typeDeclarator
    return t} 
    
and typeDeclarator = parse{
    let! t = typeSpec
    do! spaces1
    let! dec = declarator
    return Typedef(t, dec)}
    
and typeSpec =
    simpleTypeSpec
    <|> constrTypeSpec
    
and simpleTypeSpec =
    baseTypeSpec
    <|> templateTypeSpec
    <|> scopedType

and baseTypeSpec =
    floatingPtType
    <|> integerType
    <|> charType
    <|> booleanType
    <|> octetType
    <|> anyType
    <|> objectType
    //<|> valueBaseType
    
and templateTypeSpec =
    sequenceType
    <|> stringType
    <|> wideStringType
    //<|> fixedPtType

and constrTypeSpec =
    structType
    <|> unionType
    <|> enumType
    
and declarators =
    sepBy declarator (pstring ",")

and declarator =
    attempt(complexDeclarator)
    <|> simpleDeclarator
    
and simpleDeclarator = parse{
    let! name = identifier
    return SimpleDec(name)}
    
and complexDeclarator = arrayDeclarator

//********************************************************************
//	基本型  //TODO: longXXXやstringXXXのような名前も読んでしまう
//********************************************************************
and floatingPtType = (floatType <|> doubleType <|> longDoubleType)
and floatType = stringReturn "float" (Primitive("float"))
and doubleType = stringReturn "double" (Primitive("double"))
and longDoubleType = stringReturn "long double" (Primitive("long double"))
    
and integerType = (signedInt <|> unsignedInt)

and signedInt =  (signedShortInt <|> signedLongInt <|> signedLongLongInt)
and signedShortInt = stringReturn "short" (Primitive("short"))
and signedLongInt = stringReturn "long" (Primitive("long"))
and signedLongLongInt = stringReturn "long long" (Primitive("long long"))

and unsignedInt = (unsignedShortInt <|> unsignedLongInt <|>  unsignedLongLongInt)
and unsignedShortInt = stringReturn "unsigned short" (Primitive("unsigned short"))
and unsignedLongInt = stringReturn "unsigned long" (Primitive("unsigned long"))
and unsignedLongLongInt = stringReturn "unsigned long long" (Primitive("unsigned long long"))
    
and charType = stringReturn "char" (Primitive("char"))
//and wideCharType = stringReturn "char" (Primitive("wchar"))
and booleanType = stringReturn "boolean" (Primitive("boolean"))
and octetType = stringReturn "octet" (Primitive("octet"))
and anyType = stringReturn "any" (Primitive("any"))
and objectType = stringReturn "Object" (Primitive("Object"))

//*******************************************************
//	Struct
//*******************************************************
and structType = parse{
    do! spaces
    let! _ = pstring "struct"
    do! spaces1
    let! name = identifier
    let! members = between (str_ws "{") (str_ws "}") memberList
    return Struct(name, members)}

and memberList = (many1 memberDec)

and memberDec = parse{
    do! spaces
    let! t = typeSpec
    do! spaces1
    let! dec = declarators
    let! _ = str_ws ";"
    return Member(t, dec)}
    
//*******************************************************
//	Union
//*******************************************************
and unionType = parse{
    let! _ = str_ws "union"
    let! name = identifier
    let! _ = str_ws "switch"
    let! t = between (str_ws "(") (str_ws ")") switchTypeSpec
    let! body = between (str_ws "{") (str_ws "}") switchBody
    return Union(name, t, body)}

and switchTypeSpec =
    integerType 
    <|> charType
    <|> booleanType
    <|> enumType
    <|> scopedType

and switchBody = many case

and case = parse{
    let! label = many caseLabel
    let! ele = elementSpec
    let! _ = str_ws ";"
    return Case(label, ele)}

and caseLabel = 
    parse{
        let! _ = pstring "case"
        do! spaces1
        let! exp = constExp
        do! spaces1
        let! _ = pstring ":"
        return exp}
    <|> parse{
        let! _ = pstring "default"
        do! spaces1
        let! _ = pstring ";"
        return Literal(["default"])}

and elementSpec = parse{
    let! t = typeSpec
    do! spaces1
    let! dec = declarator
    return Element(t,dec)}

//*******************************************************
//	Enum
//*******************************************************
and enumType = parse{
    let! _ = str_ws "enum"
    let! name = identifier
    let! list = between (str_ws "{") (str_ws "}") enumeratorList
    return Enum(name, list)}
    
and enumeratorList =
    sepBy enumerator (str_ws ",")

and enumerator = identifier .>> spaces


//*******************************************************
//	Sequence
//*******************************************************
and sequenceType = 
    attempt(parse {
        do! spaces
        let! _ = pstring "sequence"
        let! _ = str_ws "<"
        let! t = simpleTypeSpec
        let! _ = str_ws ","
        let! count = positiveIntConst 
        let! _ = str_ws ">"
        return Sequence(t, count)})
    <|> parse{
        do! spaces
        let! _ = pstring "sequence"
        let! t = between (pchar '<') (pchar '>') simpleTypeSpec
        return Sequence(t, Literal(["0"]))}
        


//*******************************************************
//	String
//*******************************************************
and stringType =
    attempt(
        parse{
            let! _ = str_ws "string"
            let! count = between (pchar '<') (pchar '>') positiveIntConst
            return String(count)
        })
    <|> stringReturn "string" (String(Literal(["0"])))

and wideStringType =
    attempt(
        parse{
            let! _ = str_ws "wstring"
            let! count = between (pchar '<') (pchar '>') positiveIntConst
            return WString(count)
        })
    <|> stringReturn "wstring" (WString(Literal(["0"])))
    

//*******************************************************
//	Array
//*******************************************************
    
and arrayDeclarator = parse{
    let! name = identifier
    let! size = fixedArraySizeList
    return ArrayDec(name, size)}

and fixedArraySizeList = many1 fixedArraySize
    
and fixedArraySize = 
    between (str_ws "[") (str_ws "]") positiveIntConst

    
and simpleDeclaratorList = many simpleDeclarator

(*
and attrDcl = parse{
    let! _ = str_ws "readonly" //TODO: ない場合もある
    let! _ = str_ws "attribute"
    let! t = paramTypeSpec
    let! dec = simpleDeclaratorList
    return Attribute(t, dec)}
*)
  
//*******************************************************
//	Exception
//*******************************************************
and exceptDcl = parse{
    do! spaces
    let! _ = pstring "exception"
    do! spaces1
    let! name = identifier 
    let! members = between (str_ws "{") (str_ws "}") memberList
    return ExceptionType(name, members)}

   
//*******************************************************
//	Operation
//*******************************************************
and opDcl = parse{
        let! attr = opAttribute <|> parse{ return null }
        do! spaces
        let! t = opTypeSpec
        do! spaces1
        let! name = identifier
        let! ps = parameterDcls
        let! exps = raisesExpr <|> parse{ return[] }
        let! context = contextExpr <|> parse{ return[] }
        return Operation(name, ps, exps, context, t, attr)}
        
and opAttribute = str_ws "oneway"

and opTypeSpec : Parser<Definition,unit> =
    stringReturn "void" Void
    <|> paramTypeSpec
    
and parameterDcls = 
    between (str_ws "(") (str_ws ")") paramDclList

and paramDclList =
    sepBy paramDcl (pstring ",")
        
    
and paramDcl = parse{
    do! spaces
    let! attr = paramAttribute
    do! spaces1
    let! t = paramTypeSpec
    do! spaces1
    let! dec = simpleDeclarator 
    return Parameter(attr, t, dec)}
    
    
and paramAttribute = pstring "inout"  <|> pstring "in" <|> pstring "out"

and scopedTypeList =
    sepBy scopedType (str_ws ",")

and raisesExpr = parse{
    let! _ = str_ws "raises"
    let! raises = between (str_ws "(") (str_ws ")") scopedTypeList
    return raises}
    
and contextExpr = parse{
    let! _ = str_ws "context"
    let! list = between (str_ws "(") (str_ws ")") stringLiteralList
    return list}
    
    
and paramTypeSpec =
    baseTypeSpec
    <|> stringType
    <|> wideStringType
    <|> scopedType
    
//and fixedPtConstType = "fixed"



