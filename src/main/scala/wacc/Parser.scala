package wacc

import parsley.Parsley
import parsley.Parsley.{attempt, lookAhead, notFollowedBy}
import parsley.implicits.character.charLift
import parsley.implicits.lift.{Lift1, Lift2, Lift3, Lift4}
import parsley.token.{Lexer, descriptions, predicate}
import parsley.token.text.Character
import parsley.expr.{Atoms, precedence, Postfix, Ops, InfixL, Prefix}
import parsley.expr.chain.postfix1
import descriptions.numeric.{NumericDesc, PlusSignPresence}
import PlusSignPresence.Optional
import descriptions.{LexicalDesc, SpaceDesc, SymbolDesc, NameDesc, text}
import text.{TextDesc, EscapeDesc}
import parsley.combinator.{sepBy, sepBy1, some, manyUntil, choice}
import parsley.character.{noneOf, stringOfMany, string, strings, spaces}

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
                "fst", "snd", "newpair", "pair",
                "null")
        ),
        nameDesc = NameDesc.plain.copy(
            identifierStart = predicate.Basic(validIdentStart),
            identifierLetter = predicate.Basic(validIdentLetter)
        ),
        numericDesc = NumericDesc.plain.copy(
            positiveSign = Optional
        ),
        textDesc = TextDesc.plain.copy(
            escapeSequences = EscapeDesc.plain.copy(
                literals = Set('0', 'b', 't', 'n', 'f', 'r', '\"', '\'', '\\')
            ),
            multiStringEnds = Set.empty,
            graphicCharacter = predicate.Basic(validChar)
        )
    )
    private def validIdentStart(c: Char) = c == '_' || (c <= 'z' && c.isLetter)
    private def validIdentLetter(c: Char) = validIdentStart(c) || c.isDigit
    private def validChar(c: Char) = !(List('\\', '\'', '\"', '\n').contains(c))
    //private val comment = symbol("#") *> manyUntil(item, endOfLine)
    //private val skipWhitespace = skipMany(whitespace <|> comment).hide
    private val lexer = new Lexer(desc)

    def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)//skipWhitespace ~> p <~ eof 

    //def symbol(s: String): Parsley[Unit] = 
    //    lexer.lexeme.symbol(s)
    val implicits = lexer.lexeme.symbol.implicits
    val identifier = lexer.lexeme.names.identifier
    val character = lexer.lexeme.text.character.ascii
    val string = lexer.lexeme.text.string.ascii

    val num = lexer.lexeme.numeric.signed.decimal32
    def lexeme[A](p: Parsley[A]): Parsley[A] = lexer.lexeme(p)
}

object Parser {
    import lexer.{fully, identifier, num, character, string, lexeme}
    import lexer.implicits.implicitSymbol

    /* General expressions */

    lazy val ident = IdentNode.lift(identifier)

    lazy val literals: Parsley[ExprNode] =
        attempt(arrayElem) <|>
        intLiter           <|>
        attempt(boolLiter) <|> 
        charLiter          <|> 
        strLiter           <|>
        ident              <|>
        pairLiter          <|>
        bracketExpr

    lazy val bracketExpr: Parsley[ExprNode] =
        "(" ~> expr <~ ")"
    
    lazy val expr: Parsley[ExprNode] =
        attempt(op) <|> literals
         //   strings("!", "-", "len", "ord", "chr") <~ spaces), expr)
    
    // lazy val binOp: Parsley[ExprNode] = 
    //     parsley.expr.precedence(expr)
    //         (
    //             (Ops(InfixL)(MulExpr <# "*")),
    //         )
    lazy val op: Parsley[ExprNode] =
        precedence(literals)(
            Ops(Prefix)("!" #> Not,
                        "-" #> Neg,
                        "len" #> Len,
                        "ord" #> Ord,
                        "chr" #> Chr),
            Ops(InfixL)("*" #> Mul,
                        "/" #> Div,
                        "%" #> Mod),
            Ops(InfixL)("+" #> Add,
                        "-" #> Sub),
            Ops(InfixL)(">=" #> GTE,
                        ">" #> GT,
                        "<=" #> LTE,
                        "<" #> LT
                        ),
            Ops(InfixL)("==" #> Eq,
                        "!=" #> IEq),
            Ops(InfixL)("&&" #> And),
            Ops(InfixL)("||" #> Or)
        )
       
    lazy val lValue = pairElem <|> attempt(arrayElem) <|> ident 

    lazy val rValue =
        arrayLiter      <|>
        newPair         <|>
        pairElem        <|>
        funcCall        <|>
        expr

    /* Types */

    lazy val generalType: Parsley[TypeNode] =
        attempt(arrayType) <|> arrayBaseType
    
    lazy val arrayBaseType: Parsley[TypeNode] =
        baseType <|> pairType

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

    // lazy val character: Parsley[Char] =
    //     escapedChar <|> ascii.noneOf('\\', '\'', '\"')
    lazy val charLiter: Parsley[CharLiterNode] = 
        CharLiterNode.lift(character)
    
    lazy val strLiter: Parsley[StrLiterNode] =
        StrLiterNode.lift(string)

    lazy val arrayType: Parsley[ArrayTypeNode] =
        postfix1(arrayBaseType, "[]" #> ArrayTypeNode)

    lazy val arrayElem: Parsley[ArrayElemNode] =
        ArrayElemNode.lift(ident, some("[" ~> expr <~ "]"))

    lazy val arrayLiter: Parsley[ArrayLiterNode] =
        "[" ~> ArrayLiterNode.lift(sepBy(expr, ",")) <~ "]"

    lazy val pairType: Parsley[PairTypeNode] =
        PairTypeNode.lift("pair(" ~> pairElemType <~ ",", pairElemType <~ ")")

    lazy val pairElemType: Parsley[PairElemTypeNode] =
        attempt(arrayType) <|> baseType <|> "pair" #> PETPairNode()

    lazy val pairLiter: Parsley[PairLiterNode] =
        "null" #> PairLiterNode()

    lazy val pairElem: Parsley[PairElemNode] =
        "fst" ~> FstNode.lift(lValue) <|> "snd" ~> SndNode.lift(lValue)

    lazy val newPair: Parsley[NewPairNode] =
        NewPairNode.lift("newpair(" ~> expr <~ ",", expr <~ ")")

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
    
    lazy val funcCall: Parsley[CallNode] =
        CallNode.lift("call" ~> ident, "(" ~> argList <~ ")")

    lazy val argList: Parsley[ArgListNode] =
        ArgListNode.lift(sepBy(expr, ","))

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
        IfNode.lift("if" ~> expr, "then" ~> stats, "else" ~> stats <~ "fi")

    lazy val whileCon =
        WhileNode.lift("while" ~> expr, "do" ~> stats <~ "done")

    lazy val beginEnd =
        BeginEndNode.lift("begin" ~> stats <~ "end")

    lazy val assignIdent: Parsley[AssignIdentNode] =
        AssignIdentNode.lift(generalType, ident <~ "=", rValue)

    lazy val valuesEqual: Parsley[ValuesEqualNode] =
        ValuesEqualNode.lift(lValue, "=" ~> rValue)

    lazy val stat: Parsley[StatNode] = lexer.lexeme(
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
        attempt(assignIdent) <|>
        valuesEqual)
    
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