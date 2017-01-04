package toolc
package eval

import ast.Trees._
import utils._

class Evaluator(ctx: Context, prog: Program) {
  import ctx.reporter._

  def eval() {
    val ectx = new MainContext
    prog.main.stats.foreach(evalStatement(_)(ectx))
  }

  def evalStatement(stmt: StatTree)(implicit ectx: EvaluationContext): Unit = stmt match {
    case Block(stats) => {
      stats.foreach (x => evalStatement(x))
    }
    
    case If(expr, thn, els) => {
      val evalued = evalExpr(expr)
      if(evalued.asBool) evalStatement(thn) 
      else if (els.isDefined) evalStatement(els.get)
    }
    
    case While(expr, stat) => {
      while (evalExpr(expr).asBool == true) evalStatement(stat)
    }
    
    case Println(expr) => {
      val value = evalExpr(expr)
      value match {
        case BoolValue(b) => println(b.toString())
        case IntValue(b) => println(b.toString())
        case StringValue(b) => println(b)
        case ArrayValue(b) => println(b.toString())
        case ObjectValue(b) => println(b.toString())
      }
    }
    
    case Assign(id, expr) => ectx.setVariable(id.value, evalExpr(expr))
    
    case ArrayAssign(id, index, expr) => ectx.getVariable(id.value).asArray.setIndex(evalExpr(index).asInt, evalExpr(expr).asInt)
    
    case DoExpr(expr) => evalExpr(expr)
  }

  
  def evalExpr(e: ExprTree)(implicit ectx: EvaluationContext): Value = e match {
    case IntLit(value) => IntValue(value)
    
    case StringLit(value) => StringValue(value)
    
    case True() => BoolValue(true)
    
    case False() => BoolValue(false)
    
    case And(lhs, rhs) => BoolValue(evalExpr(lhs).asBool && evalExpr(rhs).asBool)
    
    case Or(lhs, rhs)  => BoolValue(evalExpr(lhs).asBool || evalExpr(rhs).asBool)
    
    case Plus(lhs, rhs) => {
      val left = evalExpr(lhs)
      val right = evalExpr(rhs)
      (left, right) match{
        case (IntValue(l), IntValue(r)) => IntValue(l + r)
        case (StringValue(l), StringValue(r)) => StringValue(l + r)
        case (IntValue(l), StringValue(r)) => StringValue(l.toString() + r)
        case (StringValue(l), IntValue(r)) => StringValue(l + r.toString())
        case (_, _) => StringValue("error")
      }
    }
    
    case Minus(lhs, rhs) => {
      val left = evalExpr(lhs)
      val right = evalExpr(rhs)
      IntValue(left.asInt - right.asInt)
    }
    
    case Times(lhs, rhs) => {
      val left = evalExpr(lhs)
      val right = evalExpr(rhs)
      IntValue(left.asInt * right.asInt)
    }
    
    case Div(lhs, rhs) => {
      val left = evalExpr(lhs)
      val right = evalExpr(rhs)
      IntValue(left.asInt / right.asInt)
    }
    
    case LessThan(lhs, rhs) => {
      val left = evalExpr(lhs)
      val right = evalExpr(rhs)
      BoolValue(left.asInt < right.asInt)
    }
    
    case Not(expr) => {
      val evalued = evalExpr(expr)
      BoolValue(!evalued.asBool)
    }
    
    case Equals(lhs, rhs) => {
      val left = evalExpr(lhs)
      val right = evalExpr(rhs)
      (left, right) match{
        case (IntValue(l), IntValue(r)) => BoolValue(l == r)
        case (BoolValue(l), BoolValue(r)) => BoolValue(l == r)
        case (l, r) => BoolValue(l eq r)
      }
    }
    
    case ArrayRead(arr, index) => {
      val array = evalExpr(arr)
      val i = evalExpr(index)
      IntValue(array.asArray.getIndex(i.asInt))
    }
    
    case ArrayLength(arr) => {
      val array = evalExpr(arr)
      IntValue(array.asArray.length)
    }
    
    case MethodCall(obj, meth, args) => {
      val o = evalExpr(obj).asObject
      val methodCallContext = new MethodContext(o)
      val method = findMethod(o.cd, meth.value)
      val arguments = args.map(evalExpr(_))
      
      method.args.zip(arguments).foreach{
        case (argName, argValue) => {
          methodCallContext.declareVariable(argName.id.value)
          methodCallContext.setVariable(argName.id.value, argValue)
        }
      }
      
      method.vars.foreach{
        case VarDecl(_, id) => methodCallContext.declareVariable(id.value)
      }
      
      method.stats.foreach {
        evalStatement(_)(methodCallContext)
      }
      
      evalExpr(method.retExpr)(methodCallContext)
    }
    
    case Variable(Identifier(name)) => ectx.getVariable(name)
    
    case New(tpe) => {
      val cd = findClass(tpe.value)
      val obj = ObjectValue(cd)
        
      fieldsOfClass(cd).foreach(obj.declareField(_))
        
      obj
    }
    
    case This() => ectx match {
      case context: MethodContext => context.obj
    }
    
    case NewIntArray(size) => {
      val s = evalExpr(size)
      ArrayValue(Array.fill(s.asInt){0})
    }
  }

  abstract class EvaluationContext {
    def getVariable(name: String): Value
    def setVariable(name: String, v: Value): Unit
    def declareVariable(name: String): Unit
  }

  class MethodContext(val obj: ObjectValue) extends EvaluationContext {
    var vars = Map[String, Option[Value]]()

    def getVariable(name: String): Value = {
      vars.get(name) match {
        case Some(ov) =>
          ov.getOrElse(fatal("Uninitialized variable '"+name+"'"))
        case _ =>
          obj.getField(name)
      }
    }

    def setVariable(name: String, v: Value) {
      if (vars contains name) {
        vars += name -> Some(v)
      } else {
        obj.setField(name, v)
      }
    }

    def declareVariable(name: String) {
      vars += name -> None
    }
  }

  class MainContext extends EvaluationContext {
    private def unavailable = fatal("The main object contains no variables and/or fields")
    def getVariable(name: String): Value          = unavailable
    def setVariable(name: String, v: Value): Unit = unavailable
    def declareVariable(name: String): Unit       = unavailable
  }

  def findMethod(cd: ClassDecl, name: String): MethodDecl = {
    cd.methods.find(_.id.value == name).orElse(
      cd.parent.map(p => findMethod(findClass(p.value), name))
    ).getOrElse(fatal("Unknown method "+cd.id+"."+name))
  }

  def findClass(name: String): ClassDecl = {
    prog.classes.find(_.id.value == name).getOrElse(fatal("Unknown class '"+name+"'"))
  }

  def fieldsOfClass(cl: ClassDecl): Set[String] = {
    cl.vars.map(_.id.value).toSet ++
      cl.parent.map(p => fieldsOfClass(findClass(p.value))).getOrElse(Set())
  }

  sealed abstract class Value {
    private def expected(tp: String) = fatal(s"Unexpected value: found $this, expected $tp")

    def asInt: Int            = expected("Int")
    def asString: String      = expected("String")
    def asBool: Boolean       = expected("Boolean")
    def asObject: ObjectValue = expected("Object")
    def asArray: ArrayValue   = expected("Array")
  }

  case class ObjectValue(cd: ClassDecl) extends Value {
    var fields = Map[String, Option[Value]]()

    def setField(name: String, v: Value) {
      if (fields contains name) {
        fields += name -> Some(v)
      } else {
        fatal(s"Unknown field '$name'")
      }
    }

    def getField(name: String) = {
      fields.get(name) match {
        case Some(Some(v)) => v
        case Some(None) => fatal(s"Field '$name' has not been initialized")
        case None => fatal(s"Unknown field '$name'")
      }
    }

    def declareField(name: String) {
      fields += name -> None
    }

    override def asObject = this
  }

  case class ArrayValue(entries: Array[Int]) extends Value {
    val length = entries.length

    private def checkBounds(index: Int) = {
      if (index < 0 || index >= length) {
        fatal(s"Index '$index' out of bounds (0 .. ${length-1})")
      }
    }

    def setIndex(i: Int, v: Int) {
      checkBounds(i)
      entries(i) = v
    }

    def getIndex(i: Int) = {
      checkBounds(i)
      entries(i)
    }

    override def asArray = this
  }

  case class StringValue(v: String) extends Value {
    override def asString = v
  }

  case class IntValue(v: Int) extends Value {
    override def asInt = v
  }

  case class BoolValue(v: Boolean) extends Value {
    override def asBool = v
  }
}
