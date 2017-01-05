package toolc
package lexer

import utils._
import scala.io.Source
import java.io.File

object Lexer extends Pipeline[File, Iterator[Token]] {
  import Tokens._

  /** Maps a string s to the corresponding keyword,
    * or None if it corresponds to no keyword
    */
  private def keywords(s: String): Option[Token] = s match {
    case "program" => Some(PROGRAM())
    case "class"   => Some(CLASS())
    case "def"     => Some(DEF())
    case "var"     => Some(VAR())
    case "String"  => Some(STRING())
    case "extends" => Some(EXTENDS())
    case "Int"     => Some(INT())
    case "Bool"    => Some(BOOLEAN())
    case "while"   => Some(WHILE())
    case "if"      => Some(IF())
    case "else"    => Some(ELSE())
    case "return"  => Some(RETURN())
    case "length"  => Some(LENGTH())
    case "true"    => Some(TRUE())
    case "false"   => Some(FALSE())
    case "this"    => Some(THIS())
    case "new"     => Some(NEW())
    case "println" => Some(PRINTLN())
    case "do"      => Some(DO())
    case _         => None
  }


  /** Reads the contents of a file, caching two characters at a time.
    * That way we can have a 2-character lookahead with
    * currentChar and nextChar
    */
  private class SourceReader(f: File) {
    private val source = Source.fromFile(f)

    /** We use this character to mark the end of the input stream. */
    val EndOfFile: Char = java.lang.Character.MAX_VALUE
    
    private var currentChar_ : Char = _
    private var nextChar_ : Char = _
    private var currentPos_ : Positioned = _
    private var nextPos_ : Positioned = _

    /** The current character */
    def currentChar = currentChar_
    /** The next character */
    def nextChar = nextChar_
    /** The position of the current character */
    def currentPos = currentPos_

    private def readChar(): Char = if (source.hasNext) {
      source.next
    } else {
      EndOfFile
    }

    /** Consumes a character from the input.
      * nextChar becomes currentChar,
      * nextChar points to the first unread character.
      */
    def consume() = {
      currentChar_ = nextChar_
      currentPos_ = nextPos_
      nextChar_ = readChar()
      nextPos_ = new Positioned{}.setPos(f, source.pos)
    }

    /** Consume n characters */
    def consume(n: Int): Unit = for (i <- 1 to n) consume()

    // To start, read the first two characters of the file
    consume(2)
  }


  def run(ctx: Context)(f: File): Iterator[Token] = {
    import ctx.reporter._

    val reader = new SourceReader(f)
    import reader._

    /** Gets rid of whitespaces and comments and calls readToken to get the next token. */
    @scala.annotation.tailrec
    def nextToken(): Token = {
      
      //skip spaces but not linejumps linejumps
      while (Character.isWhitespace(currentChar) && currentChar != '\n' && currentChar != '\r') {
        consume()
      }
      
      if (currentChar == '/' && nextChar == '/') {
        consume(2)
        // Skip until EOL
        while(currentChar != '\n' && currentChar != '\r' && currentChar != EndOfFile) consume()
        nextToken()
      } else if (currentChar == '/' && nextChar == '*') {
        consume(2)
        while(!((currentChar == '*' && nextChar == '/') || currentChar == EndOfFile)) consume()
        if (currentChar == EndOfFile) ctx.reporter.fatal("Unclosed comments", currentPos)
        else consume(2)
        nextToken()
      } else {
        readToken()
      }
    }

    /** Reads the next token from the stream. */
    def readToken(): Token = {
      // The position at the beginning of the token.
      val tokenPos = currentPos
      
      def takeStringWhile(f: Char => Boolean): String = {
        var s = ""
        while(f(currentChar)){
          s += currentChar
          consume()
        }
        s
      }
      
      def isNameValable(c: Char): Boolean = c.isLetterOrDigit || c == '_'
      
      def isStringValable(c: Char): Boolean = c != '\"' && c != '\n' && c != '\r' && c != EndOfFile
      
      def sendSimpleToken(token: Token): Token = {
        consume()
        token.setPos(tokenPos)
      }
                  
      currentChar match{
        
        //simple special characters
        case EndOfFile => EOF().setPos(tokenPos)
        case '\n' | '\r' => sendSimpleToken(LINEJUMP())
        case ':' => sendSimpleToken(COLON())
        case ';' => sendSimpleToken(SEMICOLON())
        case '.' => sendSimpleToken(DOT())
        case ',' => sendSimpleToken(COMMA())
        case '!' => sendSimpleToken(BANG())
        case '(' => sendSimpleToken(LPAREN())
        case ')' => sendSimpleToken(RPAREN())
        case '[' => sendSimpleToken(LBRACKET())
        case ']' => sendSimpleToken(RBRACKET())
        case '{' => sendSimpleToken(LBRACE())
        case '}' => sendSimpleToken(RBRACE())
        case '<' => sendSimpleToken(LESSTHAN())
        case '+' => sendSimpleToken(PLUS())
        case '-' => sendSimpleToken(MINUS())
        case '*' => sendSimpleToken(TIMES())
        case '/' => sendSimpleToken(DIV())
        
        //simple pairs of specials characters and '='
        case '&' => {
          if(nextChar == '&'){
          consume(2)
          AND().setPos(tokenPos)
          } else {
            consume()
            ctx.reporter.error("Bad token: &", currentPos)
            BAD().setPos(tokenPos)
          }
        }
        case '|' => {
          if(nextChar == '|'){
          consume(2)
          OR().setPos(tokenPos)
          } else{
            consume()
            ctx.reporter.error("Bad token: |", currentPos)
            BAD().setPos(tokenPos)
          }
        }
        case('=') => {
          if(nextChar == '='){
            consume(2)
            EQUALS().setPos(tokenPos)
          }
          else {
            consume()
            EQSIGN().setPos(tokenPos)
          }
        }
        
        //stringlit
        case('\"') => {
          consume()
          val s = takeStringWhile(isStringValable)
          currentChar match{
            case '\"' => sendSimpleToken(STRINGLIT(s))
            case EndOfFile => ctx.reporter.fatal("Unclosed string", currentPos)
            case _ => {
              ctx.reporter.error("Line jump in string literal", currentPos)
              takeStringWhile(_ != '\"')
              sendSimpleToken(BAD())
            }
          }
        }
        
        // all keywords, intlit and identifiers
        case c => {
          
          //the current char is digit
          if(c.isDigit){
            val s = takeStringWhile(_.isDigit)
            INTLIT(s.toInt).setPos(tokenPos)
          }
            
          //the current char is a letter
          else if(c.isLetter){
            val s = takeStringWhile(isNameValable)
            val k = keywords(s)
            k match {
              case Some(x) => x.setPos(tokenPos)
              case None => ID(s).setPos(tokenPos)
            }
            
          //the current char is not an valid starting char
          } else {
            val s = takeStringWhile(x => !(Character.isWhitespace(x) || x == EndOfFile))
            ctx.reporter.error(s + " is a bad token: unknown", currentPos)
            BAD().setPos(tokenPos)
          }
        }
      }
    }

    new Iterator[Token] {
      var tokenCache: Token = nextToken()
      var reachedEnd = false

      def hasNext = !reachedEnd

      def next = {
        val r = tokenCache
        if (r == EOF()) {
          reachedEnd = true
        } else {
          tokenCache = nextToken()
        }
        r
      }
    }
  }
}

/** Reads and displays the tokens, then returns a fresh iterator with the same tokens. */
object DisplayTokens extends Pipeline[Iterator[Token], Iterator[Token]] {
  def run(ctx: Context)(tokens: Iterator[Token]): Iterator[Token] = {
    val l = tokens.toList
    l foreach { t => println(s"$t(${t.line}:${t.col})") }
    l.iterator
  }
}
