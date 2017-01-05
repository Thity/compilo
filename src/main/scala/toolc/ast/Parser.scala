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
    'MainObject ::= PROGRAM() ~ 'J ~ 'Identifier ~ LBRACE() ~ 'J ~ 'Stmts ~ RBRACE() ~ 'J,
    'Stmts ::= 'Statement ~ 'Stmts | epsilon(),
    'ClassDecls ::= 'ClassDeclaration ~ 'ClassDecls | epsilon(),
    'ClassDeclaration ::= CLASS() ~ 'J ~ 'Identifier ~ 'OptExtends ~ 'ClassBody,
    'OptExtends ::= epsilon() | EXTENDS() ~ 'J ~ 'Identifier,
    'ClassBody ::= LBRACE() ~ 'J ~ 'VarDecs ~ 'MethodDecs ~ RBRACE() ~ 'J,
    'VarDecs ::= 'VarDeclaration ~ 'VarDecs | epsilon(),
    'VarDeclaration ::= VAR() ~ 'Param ~ 'InstructionEnd,
    'MethodDecs ::= 'MethodDeclaration ~ 'MethodDecs | epsilon(),
    'MethodDeclaration ::= DEF() ~ 'J ~ 'Identifier ~ LPAREN() ~ 'Params ~ RPAREN() ~
          'J ~ COLON() ~ 'J ~ 'Type ~ EQSIGN() ~ 'J ~ LBRACE() ~ 'J ~ 'VarDecs ~ 'Stmts ~
          RETURN() ~ 'J ~ 'Expression ~ 'InstructionEndOpt ~ RBRACE() ~ 'J,
    'Params ::= epsilon() | 'Param ~ 'ParamList,
    'ParamList ::= epsilon() | COMMA() ~ 'J ~ 'Param ~ 'ParamList,
    'Param ::= 'Identifier ~ COLON() ~ 'J ~ 'Type,
    'Type ::= INT() ~ 'J ~ LBRACKET() ~ 'J ~ RBRACKET() ~ 'J | BOOLEAN() ~ 'J | INT() ~ 'J | STRING() ~ 'J | 'Identifier,
    'Statement ::= IF() ~ 'J ~ LPAREN() ~ 'J ~ 'Expression ~ RPAREN() ~ 'J ~ 'MatchedIf ~ 'ElseOpt
      | 'SimpleStat,
    'MatchedIf ::= IF() ~ 'J ~ LPAREN() ~ 'J ~ 'Expression ~ RPAREN() ~ 'J ~ 'MatchedIf ~ ELSE() ~ 'J ~ 'MatchedIf
      | 'SimpleStat,
    'SimpleStat ::= LBRACE() ~ 'J ~ 'Stmts ~ RBRACE() ~ 'J
      | WHILE() ~ 'J ~ LPAREN() ~ 'J ~ 'Expression ~ RPAREN() ~ 'J ~ 'MatchedIf
      | PRINTLN() ~ 'J ~ LPAREN() ~ 'J ~ 'Expression ~ RPAREN() ~ 'InstructionEnd
      | 'Identifier ~ 'IdStat
      | DO() ~ 'J ~ LPAREN() ~ 'J ~ 'Expression ~ RPAREN() ~ 'InstructionEnd,
    'IdStat ::= EQSIGN() ~ 'J ~ 'Expression ~ 'InstructionEnd
      | LBRACKET() ~ 'J ~ 'Expression ~ RBRACKET() ~ 'J ~ EQSIGN() ~ 'J ~ 'Expression ~ 'InstructionEnd,
    'ElseOpt ::= ELSE() ~ 'J ~ 'Statement | epsilon(),
    'InstructionEndOpt ::= epsilon() | 'InstructionEnd,
    'InstructionEnd ::= SEMICOLON() ~ 'J | LINEJUMP() ~ 'J,
    'Expression ::= 'Expression ~ 'Op ~ 'Expression
      | 'Expression ~ LBRACKET() ~ 'J ~ 'Expression ~ RBRACKET()
      | 'Expression ~ DOT() ~ 'J ~ LENGTH()
      | 'Expression ~ DOT() ~ 'J ~ 'Identifier ~ LPAREN() ~ 'J ~ 'Args ~ RPAREN()
      | INTLITSENT | STRINGLITSENT
      | TRUE() | FALSE() | 'Identifier | THIS()
      | NEW() ~ 'J ~ INT() ~ 'J ~ LBRACKET() ~ 'J ~ 'Expression ~ RBRACKET()
      | NEW() ~ 'J ~ 'Identifier ~ LPAREN() ~ 'J ~ RPAREN()
      | BANG() ~ 'J ~ 'Expression
      | LPAREN() ~ 'J ~ 'Expression ~ RPAREN(),
    'Args ::= epsilon() | 'Expression ~ 'ExprList,
    'ExprList ::= epsilon() | COMMA() ~ 'J ~ 'Expression ~ 'ExprList,
    'Op ::= AND() ~ 'J | OR() ~ 'J | EQUALS() ~ 'J | LESSTHAN() ~ 'J | PLUS() ~ 'J | MINUS() ~ 'J | TIMES() ~ 'J | DIV() ~ 'J,
    'Identifier ::= IDSENT,
    'J ::= LINEJUMP() ~ 'J | epsilon() //Line jumps
  ))

  // TODO: Transform this to an LL(1) grammar
  val ll1Grammar = Grammar('Program, List[Rules[Token]](
    'Program ::= 'MainObject ~ 'ClassDecls ~ EOF(),
    'MainObject ::= PROGRAM() ~ 'J ~ 'Identifier ~ LBRACE() ~ 'J ~ 'Stmts ~ RBRACE() ~ 'J,
    'Stmts ::= 'Statement ~ 'Stmts | epsilon(),
    'ClassDecls ::= 'ClassDeclaration ~ 'ClassDecls | epsilon(),
    'ClassDeclaration ::= CLASS() ~ 'J ~ 'Identifier ~ 'OptExtends ~ 'ClassBody,
    'OptExtends ::= epsilon() | EXTENDS() ~ 'J ~ 'Identifier,
    'ClassBody ::= LBRACE() ~ 'J ~ 'VarDecs ~ 'MethodDecs ~ RBRACE() ~ 'J,
    'VarDecs ::= 'VarDeclaration ~ 'VarDecs | epsilon(),
    'VarDeclaration ::= VAR() ~ 'Param ~ 'InstructionEnd,
    'MethodDecs ::= 'MethodDeclaration ~ 'MethodDecs | epsilon(), 
    'MethodDeclaration ::= DEF() ~ 'J ~ 'Identifier ~ LPAREN() ~ 'Params ~ RPAREN() ~
          'J ~ COLON() ~ 'J ~ 'Type ~ EQSIGN() ~ 'J ~ LBRACE() ~ 'J ~ 'VarDecs ~ 'Stmts ~
          RETURN() ~ 'J ~ 'Expression ~ 'InstructionEndOpt ~ RBRACE() ~ 'J,
    'Params ::= epsilon() | 'Param ~ 'ParamList,
    'ParamList ::= epsilon() | COMMA() ~ 'J ~ 'Param ~ 'ParamList,
    'Param ::= 'Identifier ~ COLON() ~ 'J ~ 'Type,
    
    
    
    //The previous implementation was not LL1 (FIRST/FIRST)
    'Type ::= INT() ~ 'TypeFollow | BOOLEAN() | STRING() | 'Identifier,
    'TypeFollow ::= LBRACKET() ~ 'J ~ RBRACKET() | epsilon(),
    //End of 'Type changes
    
    
    
    'Statement ::= IF() ~ 'J ~ LPAREN() ~ 'J ~ 'Expression ~ RPAREN() ~ 'J ~ 'MatchedIf ~ 'ElseOpt
      | 'SimpleStat,
    'MatchedIf ::= IF() ~ 'J ~ LPAREN() ~ 'J ~ 'Expression ~ RPAREN() ~ 'J ~ 'MatchedIf ~ ELSE() ~ 'J ~ 'MatchedIf
      | 'SimpleStat,
    'SimpleStat ::= LBRACE() ~ 'J ~ 'Stmts ~ RBRACE() ~ 'J
      | WHILE() ~ 'J ~ LPAREN() ~ 'J ~ 'Expression ~ RPAREN() ~ 'J ~ 'MatchedIf
      | PRINTLN() ~ 'J ~ LPAREN() ~ 'J ~ 'Expression ~ RPAREN() ~ 'InstructionEnd
      | 'Identifier ~ 'IdStat
      | DO() ~ 'J ~ LPAREN() ~ 'J ~ 'Expression ~ RPAREN() ~ 'InstructionEnd,
    'IdStat ::= EQSIGN() ~ 'J ~ 'Expression ~ 'InstructionEnd
      | LBRACKET() ~ 'J ~ 'Expression ~ RBRACKET() ~ 'J ~ EQSIGN() ~ 'J ~ 'Expression ~ 'InstructionEnd,
    'ElseOpt ::= ELSE() ~ 'J ~ 'Statement | epsilon(),
    
    
    'InstructionEndOpt ::= epsilon() | 'InstructionEnd,
    'InstructionEnd ::= SEMICOLON() ~ 'J | LINEJUMP() ~ 'J,
    
    
    
    //The previous implementation was not LL1 (left recursion),
    //This divides the operations in priority order.
    'Expression ::= 'ExprOr ~ 'OpOr,
    'OpOr ::= OR() ~ 'J ~ 'Expression | epsilon(),
    
    'ExprOr ::= 'ExprAnd ~ 'OpAnd,
    'OpAnd ::= AND() ~ 'J ~ 'ExprOr | epsilon(),
        
    'ExprAnd ::= 'ExprEq ~ 'OpEq,
    'OpEq ::= EQUALS() ~ 'J ~ 'ExprAnd | epsilon(),
    
    'ExprEq ::= 'ExprLess ~ 'OpLess,
    'OpLess ::= LESSTHAN() ~ 'J ~ 'ExprEq | epsilon(),
            
    'ExprLess ::= 'ExprPlusMinus ~ 'OpPlusMinus,
    'OpPlusMinus ::= MINUS() ~ 'J ~ 'ExprLess | PLUS() ~ 'J ~ 'ExprLess | epsilon(),
    
    'ExprPlusMinus ::= 'ExprTimesDiv ~ 'OpTimesDiv,
    'OpTimesDiv ::= TIMES() ~ 'J ~ 'ExprPlusMinus | DIV() ~ 'J ~ 'ExprPlusMinus | epsilon(),
    
    'ExprTimesDiv ::= BANG() ~ 'J ~ 'ExprTimesDiv | 'ExprBang,
    
    'ExprBang ::= 'ExprBracket ~ 'OpBracket,
    'OpBracket ::= LBRACKET() ~ 'J ~ 'Expression ~ 'J ~ RBRACKET() | epsilon(),
    
    'ExprBracket ::= 'ExprTerm ~ 'OpDot,
    
    'OpDot ::= DOT() ~ 'J ~ 'DotEnd ~ 'OpDot | epsilon(),
    'DotEnd ::= LENGTH() | 'Identifier ~ LPAREN() ~ 'J ~ 'Args ~ RPAREN(),
        
    'ExprTerm ::= INTLITSENT | STRINGLITSENT
      | TRUE() | FALSE() | 'Identifier | THIS()
      | NEW() ~ 'J ~ 'NewEnd
      | LPAREN() ~ 'J ~ 'Expression ~ 'J ~ RPAREN(),  
      
    'NewEnd ::= 'Identifier ~ LPAREN() ~ 'J ~ RPAREN()
      | INT() ~ 'J ~ LBRACKET() ~ 'J ~ 'Expression ~ 'J ~ RBRACKET(), 
   // End of 'Expression changes
      
      
      
    'Args ::= epsilon() | 'Expression ~ 'ExprList,
    'ExprList ::= epsilon() | COMMA() ~ 'J ~ 'Expression ~ 'ExprList,
    'Identifier ::= IDSENT,
    
    //Line jumps
    'J ::= LINEJUMP() ~ 'J | epsilon()
  ))
  

  def run(ctx: Context)(tokens: Iterator[Token]): Program = {
    import ctx.reporter._
    implicit val gc = new GlobalContext()
    implicit val pc = new ParseContext()
    val list = tokens.toList
    GrammarUtils.isLL1WithFeedback(ll1Grammar) match {
      case InLL1() =>
        info("Grammar is in LL1")
      case other =>
        warning(other)
    }
    val feedback = ParseTreeUtils.parseWithTrees(ll1Grammar, list)
    feedback match {
      case s: Success[Token] =>
        (new ASTConstructorLL1).constructProgram(s.parseTrees.head)
      case fdb =>
        fatal("Parsing failed: "+fdb)
    }
  }

}
