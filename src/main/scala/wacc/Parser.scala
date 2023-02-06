package wacc

import parsley.Parsley
import parsley.Parsley.{attempt, lookAhead, notFollowedBy}
import parsley.implicits.character.charLift
import parsley.implicits.lift.{Lift1, Lift2, Lift3, Lift4}
import parsley.token.{Lexer, descriptions, predicate}
import parsley.expr.{Atoms, precedence, Postfix, Ops}
import parsley.expr.chain.postfix1
import descriptions.numeric.{NumericDesc, PlusSignPresence}
import PlusSignPresence.Optional
import descriptions.{LexicalDesc, SpaceDesc, SymbolDesc, NameDesc}
import parsley.combinator.{sepBy, sepBy1, some, manyUntil, choice}
import parsley.character.{noneOf, stringOfMany}

object lexer {
    val desc = LexicalDesc.plain.copy(
        spaceDesc = SpaceDesc.plain.copy(
            commentLine = "#",
            space = predicate.Basic(Character.isWhitespace)
        ),
        symbolDesc = SymbolDesc.plain.copy(
            hardKeywords = Set(
                "begin", "skip", "end", 
                "int", "bool", "char", "string", 
                "true", "false", 
                "is", 
                "read", 
                "free", 
                "return", "exit", 
                "print", "println", 
                "if", "then", "else", "fi", 
                "while", "do", "done", 
                "call", 
                "fst", "snd", "newpair", "pair")
        ),
        nameDesc = NameDesc.plain.copy(
            identifierStart = predicate.Basic(validIdentStart),
            identifierLetter = predicate.Basic(validIdentLetter)
        ),
        numericDesc = NumericDesc.plain.copy(
            positiveSign = Optional
        )
    )
    private def validIdentStart(c: Char) = c == '_' || (c <= 'z' && c.isLetter)
    private def validIdentLetter(c: Char) = validIdentStart(c) || c.isDigit
    //private val comment = symbol("#") *> manyUntil(item, endOfLine)
    //private val skipWhitespace = skipMany(whitespace <|> comment).hide
    private val lexer = new Lexer(desc)

    def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)//skipWhitespace ~> p <~ eof 

    //def symbol(s: String): Parsley[Unit] = 
    //    lexer.lexeme.symbol(s)
    val implicits = lexer.lexeme.symbol.implicits
    val identifier = lexer.lexeme(lexer.lexeme.names.identifier)

    val num = lexer.lexeme.numeric.signed.decimal32
}

object Parser {
    import lexer.{fully, identifier, num}
    import lexer.implicits.implicitSymbol

    /* General expressions */

    lazy val ident = IdentNode.lift(identifier)

    lazy val expr: Parsley[ExprNode] =
        intLiter    <|>
        attempt(boolLiter) <|> 
        charLiter <|> 
        strLiter <|>
        attempt(ident)     <|>
        arrayElem

    lazy val lValue = ident <|> arrayElem

    lazy val rValue = expr <|> arrayLiter


    /* Types */

    lazy val generalType: Parsley[TypeNode] =
        attempt(arrayType) <|> baseType  // <|> pairType

    lazy val baseType: Parsley[BaseTypeNode] = 
        "int"    #> BaseTypeNode("int")    <|>
        "bool"   #> BaseTypeNode("bool")   <|>
        "char"   #> BaseTypeNode("char")   <|>
        "string" #> BaseTypeNode("string")

    lazy val intLiter: Parsley[IntLiterNode] =
        IntLiterNode.lift(num)
    
    lazy val boolLiter: Parsley[BoolLiterNode] = 
        "true" #> BoolLiterNode(true) <|>
        "false" #> BoolLiterNode(false)

    lazy val escapedChar = 
        "\\" ~> choice(
            '0' #> '\u0000',
            'b' #> '\b',
            't' #> '\t',
            'n' #> '\n',
            'f' #> '\f',
            'r' #> '\r',
            '\"',
            '\'',
            '\\'
        )

    lazy val character: Parsley[Char] =
        escapedChar <|> noneOf('\\', '\'', '\"')
    lazy val charLiter: Parsley[CharLiterNode] = 
        CharLiterNode.lift("'" ~> character <~ "'")
    
    lazy val strLiter: Parsley[StrLiterNode] =
        StrLiterNode.lift("\"" ~> stringOfMany(character) <~ "\"")

    lazy val arrayType: Parsley[TypeNode] =
        postfix1(baseType, "[]" #> ArrayTypeNode)

    lazy val arrayElem: Parsley[ArrayElemNode] =
        ArrayElemNode.lift(ident, some("[" ~> expr <~ "]"))

    lazy val arrayLiter: Parsley[ArrayLiterNode] =
        "[" ~> ArrayLiterNode.lift(sepBy(expr, ",")) <~ "]"


    /* Functions */
    
    lazy val param: Parsley[ParamNode] =
        ParamNode.lift(generalType, ident)

    lazy val paramList: Parsley[ParamListNode] =
        ParamListNode.lift(sepBy(param, ","))

    lazy val func: Parsley[FuncNode] = 
        FuncNode.lift(
            generalType,
            ident,
            "(" ~> paramList <~ ")",
            "is" ~> stats <~ "end")

    /* Statements */
    
    lazy val skip: Parsley[StatNode] =
        "skip" #> SkipNode()

    lazy val read =
        ReadNode.lift("read" ~> lValue)

    lazy val free =
        FreeNode.lift("free" ~> expr)

    lazy val valReturn =
        ReturnNode.lift("return" ~> expr)

    lazy val exit =
        ExitNode.lift("exit" ~> expr)

    lazy val print =
        PrintNode.lift("print" ~> expr)

    lazy val println =
        PrintlnNode.lift("println" ~> expr)

    lazy val ifCon =
        IfNode.lift("if" ~> expr, "then" ~> stat, "else" ~> stat <~ "fi")

    lazy val whileCon =
        WhileNode.lift("while" ~> expr, "do" ~> stat <~ "done")

    lazy val beginEnd =
        BeginEndNode.lift("begin" ~> stat <~ "end")

    lazy val assignIdent: Parsley[AssignIdentNode] =
        AssignIdentNode.lift(generalType, ident <~ "=", rValue)

    lazy val valuesEqual: Parsley[ValuesEqualNode] =
        ValuesEqualNode.lift(lValue <~ "=", rValue)

    lazy val stat: Parsley[StatNode] =
        skip        <|>
        read        <|>
        free        <|>
        valReturn   <|>
        exit        <|>
        println     <|>
        print       <|>
        ifCon       <|>
        whileCon    <|>
        beginEnd    <|>
        assignIdent <|>
        valuesEqual
    
    lazy val stats: Parsley[StatNode] =
        attempt(stat <~ notFollowedBy(";")) <|> statJoin

    lazy val statJoin: Parsley[StatNode] = 
        StatJoinNode.lift(sepBy1(stat, ";"))

    /* Top Level */
    
    lazy val prog: Parsley[ProgramNode] =
        "begin" ~> ProgramNode.lift(
            manyUntil(func, lookAhead(attempt(stat))), stats) <~ "end" 

    val topLevel = fully(prog)
}