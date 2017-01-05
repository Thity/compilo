package toolc
package analyzer

import ast.Trees._

import Types._
import utils._

object TypeChecking extends Pipeline[Program, Program] {

  /** Typechecking does not produce a value, but has the side effect of
   * attaching types to trees and potentially outputting error messages. */
  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._

    def tcClass(klass: ClassDecl): Unit = klass.methods.foreach(tcMethod)

    /** Type checks statements and return expression of the method */
    def tcMethod(meth: MethodDecl): Unit = {
      meth.stats.foreach(tcStat)
      tcExpr(meth.retExpr, meth.retType.getType)
    }

    /** Checks that the expression is a subtype of the ones in expectedTps.
      * If it's not, prints an error message and returns the error type.
      * Also adds missing symbols to methods in MethodCalls
      */
    def tcExpr(expr: ExprTree, expectedTps: Type*): Unit = {
      def checkOperator(lhs: ExprTree, rhs: ExprTree,name : String): Unit = {
        tcExpr(lhs,TInt,TObject) //Left hand can be anything
        lhs.getType match {
          case TClass(c) => {
            c.lookupMethod(name) match {
              case Some(ms) if ms.params.size == 1 => tcExpr(rhs,ms.params.head._2.getType)
              case _ => error(s"No match for operator ${name} for object ${c}",expr)
            }
          }
          case _ => tcExpr(rhs,TInt)
        }
      }
      expr match {
        case And(lhs, rhs) =>
          tcExpr(lhs, TBoolean)
          tcExpr(rhs, TBoolean)
        case Or(lhs, rhs) =>
          tcExpr(lhs, TBoolean)
          tcExpr(rhs, TBoolean)
        case Not(expr) =>
          tcExpr(expr, TBoolean)
        case Plus(lhs, rhs) =>
          tcExpr(lhs,TInt,TObject,TString) //Left hand can be anything
          lhs.getType match {
            case TClass(c) => {
              c.lookupMethod("plus") match {
                case Some(ms) if ms.params.size == 1 => tcExpr(rhs,ms.params.head._2.getType)
                case _ => error(s"No match for operator+ for object ${c}",expr)
              }
            }
            case _ => tcExpr(rhs,TInt,TString)
          }
        case Minus(lhs, rhs) =>
          checkOperator(lhs,rhs,"minus")
        case Times(lhs, rhs) =>
          checkOperator(lhs,rhs,"times")
        case Div(lhs, rhs) =>
          checkOperator(lhs,rhs,"divide")
        case LessThan(lhs, rhs) =>
          tcExpr(lhs, TInt)
          tcExpr(rhs, TInt)
        case Equals(lhs, rhs) =>
          lhs.getType match{
            case TClass(_) => tcExpr(rhs, TObject)
            case t => tcExpr(rhs, t)
          }
        case ArrayRead(arr, index) =>
          tcExpr(arr, TIntArray)
          tcExpr(index, TInt)
        case ArrayLength(arr) =>
          tcExpr(arr, TIntArray)
        case NewIntArray(size) =>
          tcExpr(size, TInt)
        case MethodCall(obj, meth, args) =>
          obj.getType match{
            case TClass(cs) => cs.lookupMethod(meth.value) match{
              case Some(m) =>
                meth.setSymbol(m)
              case None => error("The object doesn't contains the method", expr)
            }
            case _ => error("Type error: object of the method must have a class type", expr)
          }
        case _ =>
      }

      if (!expectedTps.toList.exists(expr.getType.isSubTypeOf)) {
        error("Type error: Expected: " + expectedTps.mkString(" or ") + s", found: ${expr.getType}", expr)
      }

    }
 
    /** Invokes tcExpr as needed in the expressions of stat */
    def tcStat(stat: StatTree): Unit = {
      stat match{
        case Block(stats) => stats.foreach(tcStat)
        case If(expr, thn, els) =>
          tcExpr(expr, TBoolean)
          tcStat(thn)
          els.foreach(tcStat)
        case While(expr, stat) =>
          tcExpr(expr,TBoolean)
          tcStat(stat)
        case Println(expr) => tcExpr(expr, TString, TInt, TBoolean)
        case Assign(id, expr) => tcExpr(expr, id.getType)
        case ArrayAssign(id, index, expr) => id.getType match {
          case TIntArray =>
            tcExpr(index, TInt)
            tcExpr(expr, TInt)
          case _ => error("Type error: you can only assign a value to an index for IntArray")
        }
        case DoExpr(e) => tcExpr(e, TInt, TBoolean, TString, TIntArray, TObject)
      }
    }
 
    prog.main.stats.foreach(tcStat)
    prog.classes.foreach(tcClass)

    prog
  }
}
