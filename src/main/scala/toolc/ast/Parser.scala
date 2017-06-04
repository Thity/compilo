package toolc
package ast

import utils._
import Trees._
import lexer._
import lexer.Tokens._
import grammarcomp.grammar._
import GrammarUtils.InLL1
import CFGrammar._
import grammarcomp.parsing._
import GrammarDSL._

object Parser extends Pipeline[Iterator[Token], Program] {

  val toolGrammar = Grammar('Program, List[Rules[Token]](
    'Program ::= 'MainObject ~ 'ClassDecls ~ EOF(),
    'MainObject ::= PROGRAM() ~ 'Identifier ~ LBRACE() ~ 'Stmts ~ RBRACE(),
    'Stmts ::= 'Statement ~ 'Stmts | epsilon(),
    'ClassDecls ::= 'ClassDeclaration ~ 'ClassDecls | epsilon(),
    'ClassDeclaration ::= CLASS() ~ 'Identifier ~ 'OptExtends ~ 'ClassBody,
    'OptExtends ::= epsilon() | EXTENDS() ~ 'Identifier,
    'ClassBody ::= LBRACE() ~ 'VarDecs ~ 'MethodDecs ~ RBRACE(),
    'VarDecs ::= 'VarDeclaration ~ 'VarDecs | epsilon(),
    'VarDeclaration ::= VAR() ~ 'Param ~ SEMICOLON(),
    'MethodDecs ::= 'MethodDeclaration ~ 'MethodDecs | epsilon(),
    'MethodDeclaration ::= DEF() ~ 'Identifier ~ 'ParamsOpt ~ COLON() ~ 'Type ~ EQSIGN() ~ LBRACE() ~ 'VarDecs ~ 'Stmts ~ RETURN() ~ 'Expression ~ SEMICOLON() ~ RBRACE(),
    'ParamsOpt ::= LPAREN() ~ 'Params ~ RPAREN() | epsilon(),
    'Params ::= epsilon() | 'Param ~ 'ParamList,
    'ParamList ::= epsilon() | COMMA() ~ 'Param ~ 'ParamList,
    'Param ::= 'Identifier ~ COLON() ~ 'Type,
    'Type ::= INT() ~ LBRACKET() ~ RBRACKET() | BOOLEAN() | INT() | STRING() | 'Identifier,
    'Statement ::= IF() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ 'MatchedIf ~ 'ElseOpt
      | 'SimpleStat,
    'MatchedIf ::= IF() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ 'MatchedIf ~ ELSE() ~ 'MatchedIf
      | 'SimpleStat,
    'SimpleStat ::= LBRACE() ~ 'Stmts ~ RBRACE()
      | WHILE() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ 'MatchedIf
      | PRINTLN() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ SEMICOLON()
      | 'Identifier ~ 'IdStat
      | DO() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ SEMICOLON(),
    'IdStat ::= EQSIGN() ~ 'Expression ~ SEMICOLON()
      | LBRACKET() ~ 'Expression ~ RBRACKET() ~ EQSIGN() ~ 'Expression ~ SEMICOLON(),
    'ElseOpt ::= ELSE() ~ 'Statement | epsilon(),
    'Expression ::= 'Expression ~ 'Op ~ 'Expression
      | 'Expression ~ LBRACKET() ~ 'Expression ~ RBRACKET()
      | BANG() ~ 'Expression
      | 'Expression ~ DOT() ~ LENGTH()
      | 'Expression ~ DOT() ~ 'Identifier ~ 'ArgsOpt
      | 'Expression ~ 'Identifier ~ 'ExprTerm
      | 'ExprTerm,
      
    'ArgsOpt ::= LPAREN() ~ 'Args ~ RPAREN() | epsilon(),

      
    'ExprTerm ::= INTLITSENT | STRINGLITSENT
      | TRUE() | FALSE() | 'Identifier | THIS()
      | NEW() ~ INT() ~ LBRACKET() ~ 'Expression ~ RBRACKET()
      | NEW() ~ 'Identifier ~ 'ParenOpt
      | LPAREN() ~ 'Expression ~ RPAREN(),
    'ParenOpt ::= LPAREN() ~ RPAREN() | epsilon(),
    'Args ::= epsilon() | 'Expression ~ 'ExprList,
    'ExprList ::= epsilon() | COMMA() ~ 'Expression ~ 'ExprList,
    'Op ::= AND() | OR() | EQUALS() | LESSTHAN() | PLUS() | MINUS() | TIMES() | DIV(),
    'Identifier ::= IDSENT
  ))

  // TODO: Transform this to an LL(1) grammar
  val ll1Grammar = Grammar('Program, List[Rules[Token]](
    'Program ::= 'MainObject ~ 'ClassDecls ~ EOF(),
    'MainObject ::= PROGRAM() ~ 'Identifier ~ LBRACE() ~ 'Stmts ~ RBRACE(),
    'Stmts ::= 'Statement ~ 'Stmts | epsilon(),
    'ClassDecls ::= 'ClassDeclaration ~ 'ClassDecls | epsilon(),
    'ClassDeclaration ::= CLASS() ~ 'Identifier ~ 'OptExtends ~ 'ClassBody,
    'OptExtends ::= epsilon() | EXTENDS() ~ 'Identifier,
    'ClassBody ::= LBRACE() ~ 'VarDecs ~ 'MethodDecs ~ RBRACE(),
    'VarDecs ::= 'VarDeclaration ~ 'VarDecs | epsilon(),
    'VarDeclaration ::= VAR() ~ 'Param ~ SEMICOLON(),
    'MethodDecs ::= 'MethodDeclaration ~ 'MethodDecs | epsilon(), 
    'MethodDeclaration ::= DEF() ~ 'Identifier ~ 'ParamsOpt ~ COLON() ~ 'Type ~ EQSIGN() ~ LBRACE() ~ 'VarDecs ~ 'Stmts ~ RETURN() ~ 'Expression ~ SEMICOLON() ~ RBRACE(),
    'ParamsOpt ::= LPAREN() ~ 'Params ~ RPAREN() | epsilon(),
    'Params ::= epsilon() | 'Param ~ 'ParamList,
    'ParamList ::= epsilon() | COMMA() ~ 'Param ~ 'ParamList,
    'Param ::= 'Identifier ~ COLON() ~ 'Type,
    
    
    'Type ::= INT() ~ 'TypeFollow | BOOLEAN() | STRING() | 'Identifier,
    'TypeFollow ::= LBRACKET() ~ RBRACKET() | epsilon(),    
    
    
    'Statement ::= IF() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ 'MatchedIf ~ 'ElseOpt
      | 'SimpleStat,
    'MatchedIf ::= IF() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ 'MatchedIf ~ ELSE() ~ 'MatchedIf
      | 'SimpleStat,
    'SimpleStat ::= LBRACE() ~ 'Stmts ~ RBRACE()
      | WHILE() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ 'MatchedIf
      | PRINTLN() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ SEMICOLON()
      | 'Identifier ~ 'IdStat
      | DO() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ SEMICOLON(),
    'IdStat ::= EQSIGN() ~ 'Expression ~ SEMICOLON()
      | LBRACKET() ~ 'Expression ~ RBRACKET() ~ EQSIGN() ~ 'Expression ~ SEMICOLON(),
    'ElseOpt ::= ELSE() ~ 'Statement | epsilon(),
    
 
    'Expression ::= 'ExprOr ~ 'OpOr,
    'OpOr ::= OR() ~ 'Expression | epsilon(),
    
    'ExprOr ::= 'ExprAnd ~ 'OpAnd,
    'OpAnd ::= AND() ~ 'ExprOr | epsilon(),
        
    'ExprAnd ::= 'ExprEq ~ 'OpEq,
    'OpEq ::= EQUALS() ~ 'ExprAnd | epsilon(),
    
    'ExprEq ::= 'ExprLess ~ 'OpLess,
    'OpLess ::= LESSTHAN() ~ 'ExprEq | epsilon(),
            
    'ExprLess ::= 'ExprPlusMinus ~ 'OpPlusMinus,
    'OpPlusMinus ::= MINUS() ~ 'ExprLess | PLUS() ~ 'ExprLess | epsilon(),
    
    'ExprPlusMinus ::= 'ExprTimesDiv ~ 'OpTimesDiv,
    'OpTimesDiv ::= TIMES() ~ 'ExprPlusMinus | DIV() ~ 'ExprPlusMinus | epsilon(),
    
    'ExprTimesDiv ::= BANG() ~ 'ExprTimesDiv | 'ExprBang,
    
    'ExprBang ::= 'ExprBracket ~ 'OpBracket,
    'OpBracket ::= LBRACKET() ~ 'Expression ~ RBRACKET() | epsilon(),
    
    'ExprBracket ::= 'ExprTerm ~ 'OpDot,
    
    'OpDot ::= DOT() ~ 'DotEnd ~ 'OpDot | 'Identifier ~ 'ExprTerm ~ 'OpDot | epsilon(),

    'DotEnd ::= LENGTH() | 'Identifier ~ 'ArgsOpt,
    
    'ArgsOpt ::= LPAREN() ~ 'Args ~ RPAREN() | epsilon(),
        
    'ExprTerm ::= INTLITSENT | STRINGLITSENT
      | TRUE() | FALSE() | 'Identifier | THIS()
      | NEW() ~ 'NewEnd
      | LPAREN() ~ 'Expression ~ RPAREN(),  
      
    'NewEnd ::= 'Identifier ~ 'ParenOpt
      | INT() ~ LBRACKET() ~ 'Expression ~ RBRACKET(),
    
    'ParenOpt ::= LPAREN() ~ RPAREN() | epsilon(),
      
    
    'Args ::= epsilon() | 'Expression ~ 'ExprList,
    'ExprList ::= epsilon() | COMMA() ~ 'Expression ~ 'ExprList,
    'Identifier ::= IDSENT
  ))
  
  
  /* 
   * Transforms the list of Tokens to apply semicolon inference, following the Scala rules
   * http://jittakal.blogspot.ch/2012/07/scala-rules-of-semicolon-inference.html
   * 
   * these rules doesn't work for multiple lines "if-s" without braces, we added this case to the rules
   */
  def semicolonInference(list: List[Token]): List[Token] = {
    
    // remove a line jump if it doesn't precedes a starting token
    def startingJumpsOperator(token: Token, list: List[Token]): List[Token] = {
      if(list.isEmpty) List(token)
      else if(token == LINEJUMP()){
        list.head match{
          //starting tokens
          case ID(_)|DEF()|VAR()|IF()|ELSE()|LBRACE()|RBRACE()|WHILE()|PRINTLN()|DO()|RETURN() =>
            token :: list
          case _ => list
        }
      }
      else token :: list
    }
    
    // remove a line jump if it doesn't precedes an ending token
    // (reverse list for "follow")
    def endingJumpsOperator(list: List[Token], token: Token): List[Token] = {
      if(list.isEmpty) List(token)
      else if(token == LINEJUMP()){
        list.head match{
          //ending tokens
          case ID(_)|INTLIT(_)|STRINGLIT(_)|RPAREN()|RBRACKET()|LENGTH()|TRUE()|FALSE()|THIS()|INT()|BOOLEAN()|STRING() =>
            token :: list
          case _ => list
        }
      }
      else token :: list
    }
    
    // remove a line jump if it is contained in () or [] and turns it to semicolon else, unless it is after "if(...)"
    // @param ifParenLevel : -1 if not in a if condition, else equal to the parenthesis level of the if
    def bracketParenContainment(list: List[Token]): List[Token] = {
      def bracketParenContainmentAcc(reversedOutput: List[Token], input: List[Token], parenLevel: Int, bracketLevel: Int, ifParenLevel: Int): List[Token] = input match{
          case Nil => reversedOutput.reverse
          case head :: tail =>
            head match {
              case LINEJUMP() => (parenLevel, bracketLevel) match{
                case (0, 0) => bracketParenContainmentAcc(SEMICOLON()::reversedOutput, tail, parenLevel, bracketLevel, ifParenLevel)
                case _ => bracketParenContainmentAcc(reversedOutput, tail, parenLevel, bracketLevel, ifParenLevel)
              }
                
              case LPAREN() => bracketParenContainmentAcc(head::reversedOutput, tail, parenLevel+1, bracketLevel, ifParenLevel)
              case RPAREN() =>
                //if closing the if-condition
                if(parenLevel == ifParenLevel+1){
                  //if a linejump is following, remove it
                  if(!tail.isEmpty && tail.head == LINEJUMP()) bracketParenContainmentAcc(head::reversedOutput, tail.tail, parenLevel-1, bracketLevel, -1)
                  else bracketParenContainmentAcc(head::reversedOutput, tail, parenLevel-1, bracketLevel, -1)
                }
                else bracketParenContainmentAcc(head::reversedOutput, tail, parenLevel-1, bracketLevel, ifParenLevel)
              case LBRACKET() => bracketParenContainmentAcc(head::reversedOutput, tail, parenLevel, bracketLevel+1, ifParenLevel)
              case RBRACKET() => bracketParenContainmentAcc(head::reversedOutput, tail, parenLevel, bracketLevel-1, ifParenLevel)
              
              case IF() => bracketParenContainmentAcc(head::reversedOutput, tail, parenLevel, bracketLevel, parenLevel)
              case _ => bracketParenContainmentAcc(head::reversedOutput, tail, parenLevel, bracketLevel, ifParenLevel)
            }
      }
      
      bracketParenContainmentAcc(Nil, list, 0, 0, -1)
    }
    
    // add a line jump before each '}' to separate them from statements
    val step1 = list.flatMap {x => if(x == RBRACE()) List(LINEJUMP(), x) else List(x)}

    // remove a line jump if it doesn't precedes a starting token
    val step2 = step1.foldRight(List.empty[Token])(startingJumpsOperator)
    
    // remove a line jump if it doesn't follow an ending token
    val step3 = step2.foldLeft(List.empty[Token])(endingJumpsOperator).reverse
    
    // remove a line jump if it is contained in () or [] and turns it to semicolon else
    bracketParenContainment(step3)
  }
  

  def run(ctx: Context)(tokens: Iterator[Token]): Program = {
    import ctx.reporter._
    implicit val gc = new GlobalContext()
    implicit val pc = new ParseContext()
    
    val list = tokens.toList
    
    val newList = semicolonInference(list)
    
    GrammarUtils.isLL1WithFeedback(ll1Grammar) match {
      case InLL1() =>
        info("Grammar is in LL1")
      case other =>
        warning(other)
    }
    val feedback = ParseTreeUtils.parseWithTrees(ll1Grammar, newList)
    feedback match {
      case s: Success[Token] =>
        (new ASTConstructorLL1).constructProgram(s.parseTrees.head)
      case fdb =>
        fatal("Parsing failed: "+fdb)
    }
  }
}
