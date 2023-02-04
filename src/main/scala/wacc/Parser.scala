package wacc

import parsley.Parsley
import parsley.Parsley.{attempt, lookAhead}
import parsley.implicits.character.charLift
import parsley.implicits.lift.{Lift1, Lift2, Lift3, Lift4}
import parsley.token.{Lexer, descriptions, predicate}
import descriptions.numeric.{NumericDesc, PlusSignPresence}
import PlusSignPresence.Optional
import descriptions.{LexicalDesc, SpaceDesc, SymbolDesc, NameDesc}
import parsley.combinator.{attemptChoice, sepBy, sepBy1, many, manyUntil, option}

object lexer {
    val desc = LexicalDesc.plain.copy(
        spaceDesc = SpaceDesc.plain.copy(
            commentLine = "#",
            space = predicate.Basic(Character.isWhitespace)
        ),
        symbolDesc = SymbolDesc.plain.copy(
            hardKeywords = Set("begin", "skip", "end", "int", "bool", "char", "string")
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

    lazy val ident = IdentNode.lift(identifier)

    lazy val baseType = 
        "int"    #> BaseTypeNode("int")    <|>
        "bool"   #> BaseTypeNode("bool")   <|>
        "char"   #> BaseTypeNode("char")   <|>
        "string" #> BaseTypeNode("string")
    
    lazy val intLiter: Parsley[IntLiterNode] =
        IntLiterNode.lift(num)

    lazy val assignIdent: Parsley[AssignIdentNode] =
        AssignIdentNode.lift(baseType, ident <~ "=", intLiter)

    lazy val param: Parsley[ParamNode] =
        ParamNode.lift(baseType, ident)

    lazy val paramList: Parsley[ParamListNode] =
        ParamListNode.lift(sepBy(param, ","))

    lazy val func: Parsley[FuncNode] = 
        FuncNode.lift(
            baseType,
            ident,
            "(" ~> paramList <~ ")",
            "is" ~> stat <~ "end")

    lazy val statSkip: Parsley[StatNode] =
        "skip" #> SkipNode()

    lazy val stat: Parsley[StatNode] =
        statSkip <|> assignIdent

    lazy val statJoin = 
        StatJoinNode.lift(sepBy1(stat, ";"))

    lazy val prog: Parsley[ProgramNode] =
        ProgramNode.lift("begin" ~> many(func), stat <|> statJoin <~ "end")

    val topLevel = fully(prog)
}