package toolc
package ast

import ast.Trees._
import lexer.Token
import lexer.Tokens._
import grammarcomp.parsing._
import toolc.utils.Positioned

class ASTConstructorLL1 extends ASTConstructor {
  
  override def constructType(ptree: NodeOrLeaf[Token]): TypeTree = {
    ptree match {
      case Node('Type ::= _, List(Leaf(i@INT()), typeFollow)) =>
        typeFollow match {
          case Node(_, List()) => IntType().setPos(i)
          case Node('TypeFollow ::= List(LBRACKET(), 'J, RBRACKET()), List(_, _)) => 
            IntArrayType().setPos(i)
        }
      case Node('Type ::= _, List(Leaf(b@BOOLEAN()))) =>
        BooleanType().setPos(b)
      case Node('Type ::= _, List(Leaf(s@STRING()))) =>
        StringType().setPos(s)
      case Node('Type ::= List('Identifier), List(id)) =>
        val pid = constructId(id)
        ClassType(pid).setPos(pid)
    }
  }
  
  
  override def constructExpr(ptree: NodeOrLeaf[Token]): ExprTree = {
    ptree match {
      case Node('Expression ::= _, List(e, o)) =>
        val exprTree = constructExpr(e)
        constructOperation(exprTree, o)
        
      case Node('ExprOr ::= _, List(e, o)) =>
        val exprTree = constructExpr(e)
        constructOperation(exprTree, o)

      case Node('ExprAnd ::= _, List(e, o)) =>
        val exprTree = constructExpr(e)
        constructOperation(exprTree, o)
      
      case Node('ExprEq ::= _, List(e, o)) =>
        val exprTree = constructExpr(e)
        constructOperation(exprTree, o)
       
      case Node('ExprLess ::= _, List(e, o)) =>
        val exprTree = constructExpr(e)
        constructOperation(exprTree, o)
       
      case Node('ExprPlusMinus ::= _, List(e, o)) =>
        val exprTree = constructExpr(e)
        constructOperation(exprTree, o)
        
      case Node('ExprTimesDiv ::= List(BANG(), 'J, 'ExprTimesDiv), List(Leaf(i@BANG()), _, e)) =>
        val expr = constructExpr(e)
        Not(expr).setPos(i)
        
      case Node('ExprTimesDiv ::= List('ExprBang), List(e)) =>
        constructExpr(e)
      
      case Node('ExprBang ::= _, List(e, o)) =>
        o match {
           case Node('OpBracket ::= _, List()) =>
             constructExpr(e)
           case Node('OpBracket ::= _, List(_, _, expr, _)) =>
             val e1 = constructExpr(e)
             val e2 = constructExpr(expr)
             ArrayRead(e1, e2).setPos(e1)
        }
        
      case Node('ExprBracket ::= _, List(e, o)) =>
        val exprTree = constructExpr(e)
        constructOperation(exprTree, o)
        
      case Node('ExprTerm ::= _, List(Leaf(i@TRUE()))) => True().setPos(i)
      case Node('ExprTerm ::= _, List(Leaf(i@FALSE()))) => False().setPos(i)
      case Node('ExprTerm ::= _, List(Leaf(i@THIS()))) => This().setPos(i)
      
      case Node('ExprTerm ::= List(INTLITSENT), List(Leaf(it@INTLIT(i)))) => 
        IntLit(i).setPos(it)
        
      case Node('ExprTerm ::= List(STRINGLITSENT), List(Leaf(st@STRINGLIT(s)))) => 
        StringLit(s).setPos(st)
        
      case Node('ExprTerm ::= List('Identifier), List(id)) =>
        val pid = constructId(id)
        Variable(pid).setPos(pid)
      
      case Node('ExprTerm ::= List(NEW(), _, 'NewEnd), List(Leaf(nt), _, e)) => constructExpr(e).setPos(nt)
      
      case Node('NewEnd ::= List('Identifier, LPAREN(), _, RPAREN()), List(id, _, _, _)) =>
        New(constructId(id))
        
      case Node('NewEnd ::= List(INT(), _, LBRACKET(), _, 'Expression, _, RBRACKET()), List(_, _, _, _, e, _, _)) =>
        NewIntArray(constructExpr(e))
        
      case Node('ExprTerm ::= List(LPAREN(), _, 'Expression, _, RPAREN()), List(Leaf(lp), _, e, _, _)) =>
        constructExpr(e).setPos(lp)
    }
  }
  
  
  def constructOperation(e: ExprTree, o: NodeOrLeaf[Token]): ExprTree = {
    o match {
      case Node(_, List()) => e
      
      case Node(_, List(Leaf(op), _, expr)) => expr match {
        case Node(_, List(e2, o2)) =>
          val expression = constructExpr(e2)
          val left = constructOpTok(op)(e, expression).setPos(e)
          constructOperation(left, o2)
          
        case _ => 
          val expression = constructExpr(expr)
          constructOpTok(op)(e, expression).setPos(e)
      }
        
        
      case Node('OpDot ::= List(DOT(), _, 'DotEnd, 'OpDot), List(_, _, dotEnd, opDot)) =>
        dotEnd match{
          case Node('DotEnd ::= _, List(Leaf(i@LENGTH()))) =>
            val a = ArrayLength(e).setPos(e)
            constructOperation(a, opDot)
             
          case Node('DotEnd ::= _, List(id, _, _, args, _)) =>
            val m = MethodCall(e, constructId(id), constructList(args, constructExpr, hasComma = true)).setPos(e)
            constructOperation(m, opDot)
        }
    }
  }
  
  def constructOpTok(tok: Token): (ExprTree, ExprTree) => ExprTree = {
    (tok: @unchecked) match {
      case AND()      => And
      case OR()       => Or
      case EQUALS()   => Equals
      case LESSTHAN() => LessThan
      case PLUS()     => Plus
      case MINUS()    => Minus
      case TIMES()    => Times
      case DIV()      => Div
    }
  }
}