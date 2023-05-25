package dotty.tools
package dotc
package parsing

import core.Contexts.*
import core.Names.SimpleName
import Scanners.*
import util.SourceFile
import JavaTokens.*
import scala.annotation.{switch, tailrec}
import util.Chars.*
import PartialFunction.cond
import core.Decorators.em

object JavaScanners:

  class JavaScanner(source: SourceFile, override val startFrom: Offset = 0)(
      using Context
  ) extends ScannerCommon(source):

    override def decodeUni: Boolean = true

    def toToken(name: SimpleName): Token =
      val idx = name.start
      if idx >= 0 && idx <= lastKeywordStart then kwArray(idx) else IDENTIFIER

    private class JavaTokenData0 extends TokenData

    /** we need one token lookahead
      */
    val next: TokenData = new JavaTokenData0
    val prev: TokenData = new JavaTokenData0

    // Get next token ------------------------------------------------------------

    def nextToken(): Unit =
      if next.token == EMPTY then
        lastOffset = lastCharOffset
        fetchToken()
      else
        this.copyFrom(next)
        next.token = EMPTY

    def lookaheadToken: Int =
      lookAhead()
      val t = token
      reset()
      t

    def lookAhead() =
      prev.copyFrom(this)
      nextToken()

    def reset() =
      next.copyFrom(this)
      this.copyFrom(prev)

    class LookaheadScanner
        extends JavaScanner(source, startFrom = charOffset - 1):
      override protected def initialize(): Unit = nextChar()

    /** read next token
      */
    private def fetchToken(): Unit =
      offset = charOffset - 1
      ch match
        case ' ' | '\t' | CR | LF | FF =>
          nextChar()
          fetchToken()
        case _ =>
          (ch: @switch) match
            case 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' |
                'K' | 'L' | 'M' | 'N' | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T' |
                'U' | 'V' | 'W' | 'X' | 'Y' | 'Z' | '$' | '_' | 'a' | 'b' |
                'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' |
                'm' | 'n' | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' |
                'w' | 'x' | 'y' | 'z' =>
              putChar(ch)
              nextChar()
              getIdentRest()

            case '0' =>
              putChar(ch)
              nextChar()
              if ch == 'x' || ch == 'X' then
                nextChar()
                base = 16
              else base = 8
              getNumber()

            case '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
              base = 10
              getNumber()

            case '\"' =>
              nextChar()
              if ch != '\"' then // "..." non-empty string literal
                while ch != '\"' && (isUnicodeEscape || ch != CR && ch != LF && ch != SU)
                do getlitch()
                if ch == '\"' then
                  token = STRINGLIT
                  setStrVal()
                  nextChar()
                else error(em"unclosed string literal")
              else
                nextChar()
                if ch != '\"' then // "" empty string literal
                  token = STRINGLIT
                  setStrVal()
                else
                  nextChar()
                  getTextBlock()

            case '\'' =>
              nextChar()
              getlitch()
              if ch == '\'' then
                nextChar()
                token = CHARLIT
                setStrVal()
              else error(em"unclosed character literal")

            case '=' =>
              token = EQUALS
              nextChar()
              if ch == '=' then
                token = EQEQ
                nextChar()

            case '>' =>
              token = GT
              nextChar()
              if ch == '=' then
                token = GTEQ
                nextChar()
              else if ch == '>' then
                token = GTGT
                nextChar()
                if ch == '=' then
                  token = GTGTEQ
                  nextChar()
                else if ch == '>' then
                  token = GTGTGT
                  nextChar()
                  if ch == '=' then
                    token = GTGTGTEQ
                    nextChar()

            case '<' =>
              token = LT
              nextChar()
              if ch == '=' then
                token = LTEQ
                nextChar()
              else if ch == '<' then
                token = LTLT
                nextChar()
                if ch == '=' then
                  token = LTLTEQ
                  nextChar()

            case '!' =>
              token = BANG
              nextChar()
              if ch == '=' then
                token = BANGEQ
                nextChar()

            case '~' =>
              token = TILDE
              nextChar()

            case '?' =>
              token = QMARK
              nextChar()

            case ':' =>
              token = COLONop
              nextChar()

            case '@' =>
              token = AT
              nextChar()

            case '&' =>
              token = AMP
              nextChar()
              if ch == '&' then
                token = AMPAMP
                nextChar()
              else if ch == '=' then
                token = AMPEQ
                nextChar()

            case '|' =>
              token = BAR
              nextChar()
              if ch == '|' then
                token = BARBAR
                nextChar()
              else if ch == '=' then
                token = BAREQ
                nextChar()

            case '+' =>
              token = PLUS
              nextChar()
              if ch == '+' then
                token = PLUSPLUS
                nextChar()
              else if ch == '=' then
                token = PLUSEQ
                nextChar()

            case '-' =>
              token = MINUS
              nextChar()
              if ch == '-' then
                token = MINUSMINUS
                nextChar()
              else if ch == '=' then
                token = MINUSEQ
                nextChar()

            case '*' =>
              token = ASTERISK
              nextChar()
              if ch == '=' then
                token = ASTERISKEQ
                nextChar()

            case '/' =>
              nextChar()
              if !skipComment() then
                token = SLASH
                nextChar()
                if ch == '=' then
                  token = SLASHEQ
                  nextChar()
              else fetchToken()

            case '^' =>
              token = HAT
              nextChar()
              if ch == '=' then
                token = HATEQ
                nextChar()

            case '%' =>
              token = PERCENT
              nextChar()
              if ch == '=' then
                token = PERCENTEQ
                nextChar()

            case '.' =>
              token = DOT
              nextChar()
              if '0' <= ch && ch <= '9' then
                putChar('.');
                getFraction()
              else if ch == '.' then
                nextChar()
                if ch == '.' then
                  nextChar()
                  token = DOTDOTDOT
                else error(em"`.` character expected")

            case ';' =>
              token = SEMI
              nextChar()

            case ',' =>
              token = COMMA
              nextChar()

            case '(' =>
              token = LPAREN
              nextChar()

            case '{' =>
              token = LBRACE
              nextChar()

            case ')' =>
              token = RPAREN
              nextChar()

            case '}' =>
              token = RBRACE
              nextChar()

            case '[' =>
              token = LBRACKET
              nextChar()

            case ']' =>
              token = RBRACKET
              nextChar()

            case SU =>
              if isAtEnd then token = EOF
              else {
                error(em"illegal character")
                nextChar()
              }

            case _ =>
              if Character.isUnicodeIdentifierStart(ch) then
                putChar(ch)
                nextChar()
                getIdentRest()
              else {
                error(em"illegal character: ${ch.toInt}")
                nextChar()
              }
      end match
    end fetchToken

    protected def skipComment(): Boolean =
      @tailrec def skipLineComment(): Unit = ch match
        case CR | LF | SU =>
        case _            => nextChar(); skipLineComment()
      @tailrec def skipJavaComment(): Unit = ch match
        case SU => incompleteInputError(em"unclosed comment")
        case '*' =>
          nextChar(); if ch == '/' then nextChar() else skipJavaComment()
        case _ => nextChar(); skipJavaComment()
      ch match
        case '/' => nextChar(); skipLineComment(); true
        case '*' => nextChar(); skipJavaComment(); true
        case _   => false

    // Identifiers ---------------------------------------------------------------

    private def getIdentRest(): Unit =
      while true do
        (ch: @switch) match
          case 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K' |
              'L' | 'M' | 'N' | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' |
              'W' | 'X' | 'Y' | 'Z' | '$' | 'a' | 'b' | 'c' | 'd' | 'e' | 'f' |
              'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm' | 'n' | 'o' | 'p' | 'q' |
              'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z' | '0' | '1' |
              '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
            putChar(ch)
            nextChar()

          case '_' =>
            putChar(ch)
            nextChar()
            getIdentRest()
            return
          case SU =>
            finishNamed()
            return
          case _ =>
            if Character.isUnicodeIdentifierPart(ch) then
              putChar(ch)
              nextChar()
            else {
              finishNamed()
              return
            }

    // Literals -----------------------------------------------------------------

    /** Read next character in character or string literal.
      */
    protected def getlitch(): Unit =
      getlitch(scanOnly = false, inTextBlock = false)

    /** Read next character in character or string literal.
      *
      * @param scanOnly
      *   skip emitting errors or adding to the literal buffer
      * @param inTextBlock
      *   is this for a text block?
      */
    def getlitch(scanOnly: Boolean, inTextBlock: Boolean): Unit =
      def octal: Char =
        val leadch: Char = ch
        var oct: Int = digit2int(ch, 8)
        nextChar()
        if '0' <= ch && ch <= '7' then
          oct = oct * 8 + digit2int(ch, 8)
          nextChar()
          if leadch <= '3' && '0' <= ch && ch <= '7' then
            oct = oct * 8 + digit2int(ch, 8)
            nextChar()
        oct.asInstanceOf[Char]
      end octal
      def greatEscape: Char =
        nextChar()
        if '0' <= ch && ch <= '7' then octal
        else
          val x = ch match
            case 'b'  => '\b'
            case 's'  => ' '
            case 't'  => '\t'
            case 'n'  => '\n'
            case 'f'  => '\f'
            case 'r'  => '\r'
            case '\"' => '\"'
            case '\'' => '\''
            case '\\' => '\\'
            case CR | LF if inTextBlock =>
              if !scanOnly then nextChar()
              0
            case _ =>
              if !scanOnly then
                error("invalid escape character", charOffset - 1)
              ch
          if x != 0 then nextChar()
          x
      end greatEscape

      // begin getlitch
      val c: Char =
        if ch == '\\' then greatEscape
        else
          val res = ch
          nextChar()
          res
      if c != 0 && !scanOnly then putChar(c)
    end getlitch

    /** Read a triple-quote delimited text block, starting after the first three
      * double quotes.
      */
    private def getTextBlock(): Unit =
      // Open delimiter is followed by optional space, then a newline
      while ch == ' ' || ch == '\t' || ch == FF do nextChar()
      if ch != LF && ch != CR
      then // CR-LF is already normalized into LF by `JavaCharArrayReader`
        error(
          em"illegal text block open delimiter sequence, missing line terminator"
        )
        return
      nextChar()

      /* Do a lookahead scan over the full text block to:
       *   - compute common white space prefix
       *   - find the offset where the text block ends
       */
      var commonWhiteSpacePrefix = Int.MaxValue
      var blockEndOffset = 0
      var blockClosed = false
      var lineWhiteSpacePrefix = 0
      var lineIsOnlyWhitespace = true
      val in = LookaheadScanner()
      while !blockClosed && (isUnicodeEscape || ch != SU) do
        if in.ch == '\"' then // Potential end of the block
          in.nextChar()
          if in.ch == '\"' then
            in.nextChar()
            if in.ch == '\"' then
              blockClosed = true
              commonWhiteSpacePrefix =
                commonWhiteSpacePrefix min lineWhiteSpacePrefix
              blockEndOffset = in.charOffset - 2

          // Not the end of the block - just a single or double " character
          if !blockClosed then lineIsOnlyWhitespace = false
        else if in.ch == CR || in.ch == LF then // new line in the block
          in.nextChar()
          if !lineIsOnlyWhitespace then
            commonWhiteSpacePrefix =
              commonWhiteSpacePrefix min lineWhiteSpacePrefix
          lineWhiteSpacePrefix = 0
          lineIsOnlyWhitespace = true
        else if lineIsOnlyWhitespace && Character.isWhitespace(in.ch)
        then // extend white space prefix
          in.nextChar()
          lineWhiteSpacePrefix += 1
        else {
          lineIsOnlyWhitespace = false
          in.getlitch(scanOnly = true, inTextBlock = true)
        }

      // Bail out if the block never did have an end
      if !blockClosed then
        error(em"unclosed text block")
        return

      // Second pass: construct the literal string value this time
      while charOffset < blockEndOffset do
        // Drop the line's leading whitespace
        var remainingPrefix = commonWhiteSpacePrefix
        while remainingPrefix > 0 && ch != CR && ch != LF && charOffset < blockEndOffset
        do
          nextChar()
          remainingPrefix -= 1

        var trailingWhitespaceLength = 0
        var escapedNewline = false // Does the line end with `\`?
        while ch != CR && ch != LF && charOffset < blockEndOffset && !escapedNewline
        do
          if Character.isWhitespace(ch) then trailingWhitespaceLength += 1
          else trailingWhitespaceLength = 0

          // Detect if the line is about to end with `\`
          if ch == '\\' && cond(lookaheadChar()) { case CR | LF => true } then
            escapedNewline = true

          getlitch(scanOnly = false, inTextBlock = true)

        // Remove the last N characters from the buffer */
        def popNChars(n: Int): Unit =
          if n > 0 then
            val text = litBuf.toString
            litBuf.clear()
            val trimmed = text.substring(0, text.length - (n min text.length))
            trimmed.nn.foreach(litBuf.append)

        // Drop the line's trailing whitespace
        popNChars(trailingWhitespaceLength)

        // Normalize line terminators
        if (ch == CR || ch == LF) && !escapedNewline then
          nextChar()
          putChar('\n')
      end while

      token = STRINGLIT
      setStrVal()

      // Trailing """
      nextChar()
      nextChar()
      nextChar()
    end getTextBlock

    /** read fractional part and exponent of floating point number if one is
      * present.
      */
    protected def getFraction(): Unit =
      token = DOUBLELIT
      while '0' <= ch && ch <= '9' do
        putChar(ch)
        nextChar()
      if ch == 'e' || ch == 'E' then
        val lookahead = lookaheadReader()
        lookahead.nextChar()
        if lookahead.ch == '+' || lookahead.ch == '-' then lookahead.nextChar()
        if '0' <= lookahead.ch && lookahead.ch <= '9' then
          putChar(ch)
          nextChar()
          if ch == '+' || ch == '-' then
            putChar(ch)
            nextChar()
          while '0' <= ch && ch <= '9' do
            putChar(ch)
            nextChar()
        token = DOUBLELIT
      if ch == 'd' || ch == 'D' then
        putChar(ch)
        nextChar()
        token = DOUBLELIT
      else if ch == 'f' || ch == 'F' then
        putChar(ch)
        nextChar()
        token = FLOATLIT
      setStrVal()
    end getFraction

    /** convert name to long value
      */
    def intVal(negated: Boolean): Long =
      if token == CHARLIT && !negated then
        if strVal.length > 0 then strVal.charAt(0).toLong else 0
      else {
        var value: Long = 0
        val divider = if base == 10 then 1 else 2
        val limit: Long =
          if token == LONGLIT then Long.MaxValue else Int.MaxValue
        var i = 0
        val len = strVal.length
        while i < len do
          val d = digit2int(strVal.charAt(i), base)
          if d < 0 then
            error(em"malformed integer number")
            return 0
          if value < 0 ||
            limit / (base / divider) < value ||
            limit - (d / divider) < value * (base / divider) &&
            !(negated && limit == value * base - 1 + d)
          then
            error(em"integer number too large")
            return 0
          value = value * base + d
          i += 1
        if negated then -value else value
      }

    /** convert name, base to double value
      */
    def floatVal(negated: Boolean): Double =
      val limit: Double =
        if token == DOUBLELIT then Double.MaxValue else Float.MaxValue
      try
        val value: Double =
          java.lang.Double.valueOf(strVal.toString).nn.doubleValue()
        if value > limit then error(em"floating point number too large")
        if negated then -value else value
      catch
        case _: NumberFormatException =>
          error(em"malformed floating point number")
          0.0

    /** read a number into name and set base
      */
    protected def getNumber(): Unit =
      while digit2int(ch, if base < 10 then 10 else base) >= 0 do
        putChar(ch)
        nextChar()
      token = INTLIT
      if base <= 10 && ch == '.' then
        val lookahead = lookaheadReader()
        lookahead.nextChar()
        lookahead.ch match
          case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | 'd' |
              'D' | 'e' | 'E' | 'f' | 'F' =>
            putChar(ch)
            nextChar()
            return getFraction()
          case _ =>
            if !isIdentifierStart(lookahead.ch) then
              putChar(ch)
              nextChar()
              return getFraction()
      if base <= 10 &&
        (ch == 'e' || ch == 'E' ||
          ch == 'f' || ch == 'F' ||
          ch == 'd' || ch == 'D')
      then return getFraction()
      setStrVal()
      if ch == 'l' || ch == 'L' then
        nextChar()
        token = LONGLIT

    // Errors -----------------------------------------------------------------

    override def toString(): String = token match
      case IDENTIFIER => s"id($name)"
      case CHARLIT    => s"char($strVal)"
      case INTLIT     => s"int($strVal, $base)"
      case LONGLIT    => s"long($strVal, $base)"
      case FLOATLIT   => s"float($strVal)"
      case DOUBLELIT  => s"double($strVal)"
      case STRINGLIT  => s"string($strVal)"
      case SEMI =>
        ";"
      case COMMA =>
        ","
      case _ =>
        tokenString(token)

    /* Initialization: read first char, then first token */
    protected def initialize(): Unit =
      nextChar()
      nextToken()
    initialize()
  end JavaScanner

  private val (lastKeywordStart, kwArray) = buildKeywordArray(keywords)
end JavaScanners
