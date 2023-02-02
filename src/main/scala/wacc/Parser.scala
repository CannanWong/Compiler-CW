package wacc

import parsley.{Parsley, Success, Failure}
import parsley.combinator.sepBy1
import parsley.character.digit
import parsley.expr.chain
import parsley.implicits.character.charLift
import parsley.implicits.lift.Lift1
import parsley.token.{Lexer, descriptions, predicate}
import descriptions.{LexicalDesc, SpaceDesc, SymbolDesc}

object lexer {
    val desc = LexicalDesc.plain.copy(
        spaceDesc = SpaceDesc.plain.copy(
            commentLine = "#",
            space = predicate.Basic(Character.isWhitespace)
        ),
        symbolDesc = SymbolDesc.plain.copy(
            hardKeywords = Set("begin", "skip", "end")
        )
    )
    //private val comment = symbol("#") *> manyUntil(item, endOfLine)
    //private val skipWhitespace = skipMany(whitespace <|> comment).hide
    val lexer = new Lexer(desc)

    def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)//skipWhitespace ~> p <~ eof 

    //def symbol(s: String): Parsley[Unit] = 
    //    lexer.lexeme.symbol(s)
    val implicits = lexer.lexeme.symbol.implicits
}

object Parser {
    import lexer.fully
    import lexer.implicits.implicitSymbol

    lazy val statJoin = 
            StatJoinNode.lift(sepBy1(statSkip, ";"))
    lazy val statSkip: Parsley[StatNode] = 
            "skip" #> SkipNode()

    lazy val prog: Parsley[ProgramNode] = "begin" ~> ProgramNode.lift(statJoin) <~ "end"

    val topLevel = fully(prog)
}