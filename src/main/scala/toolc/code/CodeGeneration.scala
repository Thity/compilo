package toolc
package code

import ast.Trees._
import analyzer.Symbols._
import analyzer.Types._
import cafebabe._
import AbstractByteCodes.{New => _, _}
import ByteCodes._
import utils._

object CodeGeneration extends Pipeline[Program, Unit] {
 
  // A mapping from a parameter/local variable name to the index of this parameter/variable
  // in the fields of a method
  type LocalsPosMapping = Map[String,Int]

  def run(ctx: Context)(prog: Program): Unit = {
    import ctx.reporter._

    /**** Helper methods ****/

    def generateClassFile(ct: ClassDecl, shortFileName: String, outDir: String): Unit = {
      val cs = ct.getSymbol
      val cf = new ClassFile(cs.name, cs.parent.map(_.name))
      cf.setSourceFile(shortFileName)
      cf.addDefaultConstructor

      for(v <- ct.vars){
        val tpe = typeToDescr(v.tpe.getType)
        val name = v.id.value
        cf.addField(tpe, name)
      }
      
      for(m <- ct.methods){
        val retTpe = typeToDescr(m.retType.getType)
        val name = m.id.value
        val args = m.args.map(x => typeToDescr(x.tpe.getType))
        cGenMethod(cf.addMethod(retTpe, name, args).codeHandler ,m)
      }

      writeClassFile(cf, outDir, cs.name)
    }

    def generateMainClassFile(main: MainObject, sourceFileName: String, outDir: String): Unit = {
      // Main class has a special handling
      val cs = main.getSymbol
      val mainClassFile = new ClassFile(cs.name, None)
      mainClassFile.setSourceFile(sourceFileName)
      mainClassFile.addDefaultConstructor

      cGenMain(
        mainClassFile.addMainMethod.codeHandler,
        prog.main.stats,
        cs.name
      )

      writeClassFile(mainClassFile, outDir, cs.name)
    }

    /** Writes the proper .class file in a given directory. An empty string for dir is equivalent to "./". */
    def writeClassFile(cf: ClassFile, dir: String, className: String) = {
      try {
        cf.writeToFile(dir + className + ".class")
      } catch {
        case e: Exception => fatal(e.getMessage)
      }
    }


    def cGenMethod(ch: CodeHandler, mt: MethodDecl): Unit = {
      val methSym = mt.getSymbol

      // Maps each argument to one local variable index position
      val argMappings = mt.args.zipWithIndex.map { case (arg, index) =>
        arg.id.getSymbol.name -> (index + 1)
      }.toMap

      // Maps each variable to one local variable index position
      val variableMappings = mt.vars.map( v => v.getSymbol.name -> ch.getFreshVar).toMap

      val mapping = argMappings ++ variableMappings
      
      val cname = methSym.classSymbol.name

      for(stat <- mt.stats){
        cGenStat(stat)(ch, mapping, cname)
      }
      
      cGenExpr(mt.retExpr)(ch, mapping, cname)
      
      mt.retType.getType match{
        case TInt | TBoolean => ch << LineNumber(mt.line) << IRETURN
        case _ => ch << LineNumber(mt.line) << ARETURN
      }

      ch.freeze
    }

    // Generates code for the main method
    def cGenMain(ch: CodeHandler, stmts: List[StatTree], cname: String): Unit = {

      for(stat <- stmts){
        cGenStat(stat)(ch, Map(), cname)
      }
      
      ch << RETURN
      ch.freeze
    }


    // Generates code for a statement
    def cGenStat(statement: StatTree)
                (implicit ch: CodeHandler, mapping: LocalsPosMapping, cname: String): Unit = {
      
      ch << LineNumber(statement.line)
      
      statement match {
        case Block(stats) =>
          stats foreach cGenStat        
        case If(expr, thn, els) =>
          val elseLabel = ch.getFreshLabel("else")
          val endLabel = ch.getFreshLabel("end")
          
          cGenExpr(expr)
          
          ch << IfEq(elseLabel)
          
          //then
          cGenStat(thn)
          ch << Goto(endLabel)
          
          ch << Label(elseLabel)
          els.foreach(cGenStat)
          
          ch << Label(endLabel)
        case While(expr, stat) =>
          val againLabel = ch.getFreshLabel("again")
          val endLabel = ch.getFreshLabel("end")
                    
          ch << Label(againLabel)
          
          cGenExpr(expr)
          ch << IfEq(endLabel)
          cGenStat(stat)
          ch << Goto(againLabel)
          
          ch << Label(endLabel)
        case Println(expr) =>
          ch << GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;")
          cGenExpr(expr)
          ch << InvokeVirtual("java/io/PrintStream", "println", "(" + typeToDescr(expr.getType) + ")V")
        case Assign(id, expr) =>
          val fieldName = id.value
          mapping.get(fieldName) match{
            case Some(i) =>
              cGenExpr(expr)
              id.getType match{
                case TInt | TBoolean =>
                  ch << IStore(i)
                case _ => 
                  ch << AStore(i)
            }
            case None =>
              cGenExpr(This())
              cGenExpr(expr)
              ch << PutField(cname, fieldName, typeToDescr(id.getType))
          }
        case ArrayAssign(id, index, expr) =>
          val fieldName = id.value
          mapping.get(fieldName) match{
            case Some(i) =>
              ch << ALoad(i)
              cGenExpr(index)
              cGenExpr(expr)
              ch << IASTORE
            case None =>
              cGenExpr(This())
              ch << GetField(cname, fieldName, typeToDescr(id.getType))  
              cGenExpr(index)
              cGenExpr(expr)
              ch << IASTORE
          }
        case DoExpr(e) =>
          cGenExpr(e)
          ch << POP
        case _ =>
      }
    }

    // Generates code for an expression
    def cGenExpr(expr: ExprTree)
                (implicit ch: CodeHandler, mapping: LocalsPosMapping, cname: String): Unit = {
     
      ch << LineNumber(expr.line)
      
      expr match {
        case And(lhs,rhs) =>
          ch << ICONST_0
          cGenExpr(lhs)

          val theLabel = ch.getFreshLabel("alreadyFalse")
          ch << IfEq(theLabel)

          // Only care about the right hand side value
          ch << POP
          cGenExpr(rhs)

          ch << Label(theLabel)
        case Or(lhs,rhs) =>
          ch << ICONST_1
          cGenExpr(lhs)

          val theLabel = ch.getFreshLabel("alreadyTrue")
          ch << IfNe(theLabel)

          // Only care about the right hand side value
          ch << POP
          cGenExpr(rhs)

          ch << Label(theLabel)
        case Not(expr) =>
          ch << ICONST_1
          cGenExpr(expr)

          val theLabel = ch.getFreshLabel("false")
          ch << IfEq(theLabel)

          // Only care about the right hand side value
          ch << POP
          ch << ICONST_0

          ch << Label(theLabel)
        case Plus(lhs, rhs) =>
          (lhs.getType, rhs.getType) match {
            case (TInt, TInt) =>
              cGenExpr(lhs)
              cGenExpr(rhs)
              ch << IADD
            case (TString, TString) =>
              ch << DefaultNew("java/lang/StringBuilder")
              cGenExpr(lhs)
              ch << InvokeVirtual("java/lang/StringBuilder", "append", "(" + typeToDescr(TString) + ")Ljava/lang/StringBuilder;")
              cGenExpr(rhs)
              ch << InvokeVirtual("java/lang/StringBuilder", "append", "(" + typeToDescr(TString) + ")Ljava/lang/StringBuilder;")
              ch << InvokeVirtual("java/lang/StringBuilder", "toString", "()" + typeToDescr(TString))
            case (TInt, TString) =>
              ch << DefaultNew("java/lang/StringBuilder")
              cGenExpr(lhs)
              ch << InvokeVirtual("java/lang/StringBuilder", "append", "(" + typeToDescr(TInt) + ")Ljava/lang/StringBuilder;")
              cGenExpr(rhs)
              ch << InvokeVirtual("java/lang/StringBuilder", "append", "(" + typeToDescr(TString) + ")Ljava/lang/StringBuilder;")
              ch << InvokeVirtual("java/lang/StringBuilder", "toString", "()" + typeToDescr(TString))
            case (TString, TInt) =>
              ch << DefaultNew("java/lang/StringBuilder")
              cGenExpr(lhs)
              ch << InvokeVirtual("java/lang/StringBuilder", "append", "(" + typeToDescr(TString) + ")Ljava/lang/StringBuilder;")
              cGenExpr(rhs)
              ch << InvokeVirtual("java/lang/StringBuilder", "append", "(" + typeToDescr(TInt) + ")Ljava/lang/StringBuilder;")
              ch << InvokeVirtual("java/lang/StringBuilder", "toString", "()" + typeToDescr(TString))
            case (TClass(c), _) => cGenExpr(new MethodCall(lhs, new Identifier("plus"), List(rhs)))
            case _ =>
          }
        case Minus(lhs, rhs) =>
          lhs.getType match {
            case TClass(c) => cGenExpr(new MethodCall(lhs, new Identifier("minus"), List(rhs)))
            case _ =>
              cGenExpr(lhs)
              cGenExpr(rhs)
              ch << ISUB
          }

        case Times(lhs, rhs) =>
          lhs.getType match {
            case TClass(c) => cGenExpr(new MethodCall(lhs, new Identifier("times"), List(rhs)))
            case _ =>
              cGenExpr(lhs)
              cGenExpr(rhs)
              ch << IMUL
          }
        case Div(lhs, rhs) =>
          lhs.getType match {
            case TClass(c) => cGenExpr(new MethodCall(lhs, new Identifier("divided"), List(rhs)))
            case _ =>
              cGenExpr(lhs)
              cGenExpr(rhs)
              ch << IDIV
          }
        case LessThan(lhs, rhs) =>
          ch << ICONST_1
          cGenExpr(lhs)
          cGenExpr(rhs)
          
          val theLabel = ch.getFreshLabel("true")
          ch << If_ICmpLt(theLabel)
          
          //false
          ch << POP
          ch << ICONST_0
          
          ch << Label(theLabel)
        case Equals(lhs, rhs) =>
          (lhs.getType, rhs.getType) match {
            case (TInt, TInt) | (TBoolean, TBoolean) =>
              ch << ICONST_1
              cGenExpr(lhs)
              cGenExpr(rhs)
              
              val theLabel = ch.getFreshLabel("true")
          
              ch << If_ICmpEq(theLabel)
              
              //false
              ch << POP
              ch << ICONST_0
              
              ch << Label(theLabel)
            case _ =>
              ch << ICONST_1
              cGenExpr(lhs)
              cGenExpr(rhs)
              
              val theLabel = ch.getFreshLabel("true")
          
              ch << If_ACmpEq(theLabel)
              
              //false
              ch << POP
              ch << ICONST_0
              
              ch << Label(theLabel)
          }
        case ArrayRead(arr, index) =>
          cGenExpr(arr)
          cGenExpr(index)
          ch << IALOAD
        case ArrayLength(arr) =>
          cGenExpr(arr)
          ch << ARRAYLENGTH
        case NewIntArray(size) =>
          cGenExpr(size)
          ch << NewArray.primitive("T_INT")
        case This() => ch << ALoad(0)
        case mc@MethodCall(obj, meth, args) =>
          cGenExpr(obj)
          args.foreach(cGenExpr)

          val argsTypes = (args.map(x => typeToDescr(x.getType))).fold("")((a, b) => a + b)
          val argsTypesFormatted = "(" + argsTypes + ")"
          
          
          ch << InvokeVirtual(typeToDescr(obj.getType, true), meth.value, argsTypesFormatted + typeToDescr(mc.getType))
        case New(tpe) => ch << DefaultNew(typeToDescr(tpe.getType, true))
        case IntLit(value) => ch << Ldc(value)
        case StringLit(value) => ch << Ldc(value)
        case True() => ch << ICONST_1
        case False() => ch << ICONST_0
        case Variable(id: Identifier) =>
          val fieldName = id.value
          id.getType match{
            case TInt | TBoolean => mapping.get(fieldName) match{
              case Some(i) => ch << ILoad(i)
              case None =>
                cGenExpr(This())
                ch << GetField(cname, fieldName, typeToDescr(id.getType))
            }
            case _ => mapping.get(fieldName) match{
              case Some(i) => ch << ALoad(i)
              case None =>
                cGenExpr(This())
                ch << GetField(cname, fieldName, typeToDescr(id.getType))
            }
          }
        case _ =>
      }
    }
    

    // Transforms a Tool type to the corresponding JVM type description
    def typeToDescr(t: Type, forMethodCallObj: Boolean = false): String = (t: @unchecked) match {
      case TInt => "I"
      case TBoolean => "Z"
      case TString => "Ljava/lang/String;"
      case TIntArray => "[I"
      case TClass(sym) => if(forMethodCallObj) sym.name else "L" + sym.name + ";"
    }

    /**** Main code ****/

    // Make output directory
    val outDir = ctx.outDir.map(_.getPath+"/").getOrElse("./")
    val f = new java.io.File(outDir)
    if (!f.exists()) {
      f.mkdir()
    }

    // Name of source file (to track positions)
    val sourceName = ctx.files.head.getName

    // output class code
    prog.classes foreach {
      generateClassFile(_, sourceName, outDir)
    }

    // output main object code
    generateMainClassFile(prog.main, sourceName, outDir)
  }   
}
