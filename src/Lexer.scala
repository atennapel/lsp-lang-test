import Common.Span

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object Lexer:
  enum TokenType derives CanEqual:
    case Eof
    case Error

    case DoubleArrow
    case Equals
    case Lambda
    case ParenL
    case ParenR
    case Semicolon

    case Def
    case Else
    case If
    case Let
    case Then

    case Ident
    case Number

  final case class Token(ty: TokenType, span: Span):
    def show(text: String): String = s"$ty(${span.extract(text)})"

  final case class Tokens(tokens: ArrayBuffer[Token]):
    def show(text: String): String =
      tokens.map(_.show(text)).mkString("[", ", ", "]")

  def lex(text: String): Tokens =
    val buffer = ArrayBuffer.empty[Token]
    loop(text, buffer, State.Start, 0, 0)
    Tokens(buffer)

  private enum State:
    case Start
    case Comment
    case Number
    case Ident

  @tailrec
  private def loop(
      text: String,
      buffer: ArrayBuffer[Token],
      state: State,
      ix: Int,
      start: Int
  ): Unit =
    inline def oob(i: Int) = i >= text.length
    inline def nextIs(c: Char) = !oob(ix + 1) && text(ix + 1) == c
    inline def span(n: Int) = Span(start, ix + n)
    inline def push(ty: TokenType, span: Span): Unit =
      buffer += Token(ty, span)
    inline def go(state: State, skip: Int = 1): Unit =
      loop(text, buffer, state, ix + skip, start + skip)
    inline def goKeep(state: State, skip: Int = 1): Unit =
      loop(text, buffer, state, ix + skip, start)
    inline def skip(skip: Int = 1): Unit = go(state, skip)
    inline def keep(skip: Int = 1): Unit = goKeep(state, skip)
    inline def consume(state: State, skip: Int = 0): Unit =
      val i = ix + skip
      loop(text, buffer, state, i, i)
    inline def pushSkip(ty: TokenType, n: Int): Unit =
      push(ty, span(n)); skip(n)
    inline def finishIdent(): Unit =
      val sp = span(0)
      val ty = sp.extract(text) match
        case "def"  => TokenType.Def
        case "else" => TokenType.Else
        case "if"   => TokenType.If
        case "let"  => TokenType.Let
        case "then" => TokenType.Then
        case _      => TokenType.Ident
      push(ty, sp)
    if oob(ix) then
      state match
        case State.Start   => ()
        case State.Comment => ()
        case State.Number  => push(TokenType.Number, span(0))
        case State.Ident   => finishIdent()
      push(TokenType.Eof, Span(text.length, text.length))
    else
      state match
        case State.Start =>
          text(ix) match
            case c if c.isWhitespace => skip()
            case c if c.isDigit      => goKeep(State.Number)
            case '\\' | 'λ'          => pushSkip(TokenType.Lambda, 1)
            case c if c.isLetter     => goKeep(State.Ident)
            case '-' if nextIs('-')  => go(State.Comment, skip = 2)
            case '=' if nextIs('>')  => pushSkip(TokenType.DoubleArrow, 2)
            case '⇒'                 => pushSkip(TokenType.DoubleArrow, 1)
            case '='                 => pushSkip(TokenType.Equals, 1)
            case '('                 => pushSkip(TokenType.ParenL, 1)
            case ')'                 => pushSkip(TokenType.ParenR, 1)
            case ';'                 => pushSkip(TokenType.Semicolon, 1)
            case _                   =>
              val n = buffer.length
              (if n > 0 then buffer(n - 1) else null) match
                case last: Token
                    if last.ty == TokenType.Error && last.span.end == ix =>
                  buffer(n - 1) =
                    Token(TokenType.Error, Span(last.span.start, ix + 1))
                  skip()
                case _ => pushSkip(TokenType.Error, 1)
        case State.Comment =>
          text(ix) match
            case '\n' => go(State.Start)
            case _    => skip()
        case State.Number =>
          text(ix) match
            case c if c.isDigit => keep()
            case _ => push(TokenType.Number, span(0)); consume(State.Start)
        case State.Ident =>
          text(ix) match
            case c if c.isLetterOrDigit || c == '_' => keep()
            case _ => finishIdent(); consume(State.Start)
