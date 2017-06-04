package toolc
package analyzer

import utils._
import ast.Trees._
import Symbols._

object NameAnalysis extends Pipeline[Program, Program] {

  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._

    def collectSymbols(prog: Program): GlobalScope = {

      val global = new GlobalScope

      val mcSym = new MainSymbol(prog.main.id.value)
      global.mainClass = mcSym
      prog.main.setSymbol(mcSym)
      prog.main.id.setSymbol(mcSym)

      for (c <- prog.classes) {     
        val name = c.id.value
        
        global.lookupClass(name) match{
          case Some(x) => 
            error(s"Class with name ${x.name} already defined.", c)
          case None =>
            val symbol = new ClassSymbol(name).setPos(c)
            global.classes += ((name, symbol))
            c.setSymbol(symbol)
            c.id.setSymbol(symbol)
        }
      }
      

      // Set parent Symbols
      for {
        cls <- prog.classes
        clSym = global.classes(cls.id.value)
        par <- cls.parent
      } yield {
        global.lookupClass(par.value) match {
          case None =>
            error(s"Class ${clSym.name} extends class ${par.value} which is not defined.", par)
          case Some(parSym) =>
            clSym.parent = Some(parSym)
            par.setSymbol(parSym)
        }
      }

      // Check there are no cycles in the inheritance graph
      prog.classes foreach { cls =>
        val clsSym = cls.getSymbol

        def mkChain(curr: ClassSymbol): List[ClassSymbol] = {
          curr.parent match {
            case None => List(curr)
            case Some(`clsSym`) => List(curr, clsSym)
            case Some(p) => curr :: mkChain(p)
          }
        }

        val chain = mkChain(clsSym)

        if (chain.size > 1 && chain.head == chain.last) {
          fatal("Cyclic inheritance: " + chain.map(_.name).mkString(" -> "))
        }

      }

      // We now know that every class is unique and the inheritance graph is
      // correct. We proceed to check the contents of these classes.
      prog.classes.foreach(collectInClass)
      
      def collectInClass(c: ClassDecl): Unit = {
        
        //if has no parent, collectMethod of it and its children
        if(c.getSymbol.parent.isEmpty) collectDescending(c)
      }
      
      def collectDescending(c: ClassDecl): Unit = {
        if(c.id.value == "Object") error(s"Class with name Object is not permited", c)
        if(c.id.value == mcSym.name) error(s"Class with same name as main Object is not permited", c)
        
        setCSymbols(c, global) 
          
        //collectChildren
        for(child <- prog.classes){
          
          //if the class is a child, collectDescending of it
          child.parent match{
            case Some(p) =>
              if(p.value == c.id.value) collectDescending(child)
            case None =>
          }
        }
      }
      global
    }

    def setPSymbols(prog: Program, gs: GlobalScope): Unit = {
      for(s <- prog.main.stats){
        setSSymbols(s)(gs, None)
      }
    }

    def setCSymbols(klass: ClassDecl, gs: GlobalScope): Unit = {
      val classSym = gs.lookupClass(klass.id.value).get
      for (varDecl <- klass.vars) {
        setTypeSymbol(varDecl.tpe, gs)
        collectClassVariable(varDecl, classSym, klass)
      }

      klass.methods.foreach(setMSymbols(_, gs, classSym))
    }
    
    def collectClassVariable(v: VarDecl, symbol: ClassSymbol, cd: ClassDecl){
      val name = v.id.value
         
      symbol.lookupVar(name) match{
        case Some(s) => error(s"Variable ${name} already defined in the same class or in one of its parent.", s)
        case None =>
          val variableSymbol = new VariableSymbol(name).setPos(v)
          variableSymbol.setType(v.tpe.getType)
          symbol.members += ((name, variableSymbol))
          v.setSymbol(variableSymbol)
          v.id.setSymbol(variableSymbol)
      }
    }

    def setMSymbols(meth: MethodDecl, gs: GlobalScope, cs: ClassSymbol): Unit = {
        val name = meth.id.value
          
        cs.lookupMethod(name) match{
          
          //the method names is already defined
          case Some(methSym) =>
              
            //if in the same class, error
            if(methSym.classSymbol == cs)
              error(s"Method with name ${methSym.name} already defined in the same class.", meth)
              
            //if in parent but not same number of args, error
            else if(methSym.argList.size != meth.args.size)
              error(s"Overriding of ${methSym.name} must have the same number of arguments.", meth)
                
            //else it's an override, unless the types doesn't match
            else {
              val newMethSym = new MethodSymbol(name, cs).setPos(meth)
              
              newMethSym.overridden = Some(methSym)
              cs.methods += ((name, newMethSym))
              
              setMSymbolsHelper(meth, newMethSym, gs)
              
              //throw an error if the return type is not the same
              if(methSym.getType != meth.retType.getType)
                error(s"Overriding of ${methSym.name} must have same return type.", meth)
              
              //throw an error if same number of args but not same types
              if(methSym.argList.zip(meth.args).exists(x => x._1.getType != x._2.tpe.getType))
                error(s"Overriding of ${methSym.name} must have arguments of the same types.", meth)
              
              newMethSym.setType(meth.retType.getType)
            }
            
          //the method is a new one
          case None =>
            val newMethSym = new MethodSymbol(name, cs).setPos(meth)
            
            cs.methods += ((name, newMethSym))
            
            setMSymbolsHelper(meth, newMethSym, gs)
            
            newMethSym.setType(meth.retType.getType)
        }
    }
    
    def setMSymbolsHelper(meth: MethodDecl, methSym: MethodSymbol, gs: GlobalScope){
      meth.setSymbol(methSym)
      meth.id.setSymbol(methSym)
      setTypeSymbol(meth.retType, gs)
           
      for(a <- meth.args){
        setTypeSymbol(a.tpe, gs)
        collectMethodArgs(a, methSym)
      }
            
      for(v <- meth.vars){
        setTypeSymbol(v.tpe, gs)
        collectMethodVariable(v, methSym)
      }
            
      meth.stats.foreach(setSSymbols(_)(gs, Some(methSym)))
      setESymbols(meth.retExpr)(gs, Some(methSym))
    }
    
    def collectMethodArgs(a: Formal, symbol: MethodSymbol){
      val name = a.id.value
         
      symbol.params.get(name) match{
        case Some(arg) =>
          error(s"Argument with name ${arg.name} already exists.", arg)
        case None =>
          val variableSymbol = new VariableSymbol(name).setPos(a)
          variableSymbol.setType(a.tpe.getType)
          symbol.params += ((name, variableSymbol))
          symbol.argList = symbol.argList :+ variableSymbol
          a.setSymbol(variableSymbol)
          a.id.setSymbol(variableSymbol)
      }
    }
    
    def collectMethodVariable(v: VarDecl, symbol: MethodSymbol){
      val name = v.id.value
               
      symbol.params.get(name) match{
        case Some(p) => error(s"Variable ${p.name} already defined as an argument.", p)
        case None =>
          symbol.members.get(name) match{
            case Some(m) => error(s"Variable ${m.name} already defined in same method.", m)
            case None =>
              val variableSymbol = new VariableSymbol(name).setPos(v)
              variableSymbol.setType(v.tpe.getType)
              symbol.members += ((name, variableSymbol))
              v.setSymbol(variableSymbol)
              v.id.setSymbol(variableSymbol)
          }
      } 
    }

    def setSSymbols(stat: StatTree)(implicit gs: GlobalScope, ms: Option[MethodSymbol]): Unit = {
      stat match{
        case Block(stats: List[StatTree]) => stats.foreach(setSSymbols)
        case If(expr: ExprTree, thn: StatTree, els: Option[StatTree]) =>
          setESymbols(expr)
          setSSymbols(thn)
          if(els.isDefined) setSSymbols(els.get)
        case While(expr: ExprTree, stat: StatTree) =>
          setESymbols(expr)
          setSSymbols(stat)
        case Println(expr: ExprTree) => setESymbols(expr)
        case Assign(id: Identifier, expr: ExprTree) =>
          setISymbol(id)
          setESymbols(expr)
        case ArrayAssign(id: Identifier, index: ExprTree, expr: ExprTree) =>
          setISymbol(id)
          setESymbols(index)
          setESymbols(expr)
        case DoExpr(e: ExprTree) => setESymbols(e)
      }
    }

    def setISymbol(id: Identifier)(implicit ms: Option[MethodSymbol]) = {
      // in this context, it will always be an expression (variable)
      ms.flatMap(_.lookupVar(id.value)) match {
        case None =>
          error("Undeclared identifier: " + id.value + ".", id)
        case Some(sym) =>
          id.setSymbol(sym)
      }
    }

    def setESymbols(expr: ExprTree)(implicit gs: GlobalScope, ms: Option[MethodSymbol]): Unit = {
      expr match{
        case And(lhs: ExprTree, rhs: ExprTree) =>
          setESymbols(lhs)
          setESymbols(rhs)
        case Or(lhs: ExprTree, rhs: ExprTree) =>
          setESymbols(lhs)
          setESymbols(rhs)
        case Plus(lhs: ExprTree, rhs: ExprTree) =>
          setESymbols(lhs)
          setESymbols(rhs)
        case Minus(lhs: ExprTree, rhs: ExprTree) =>
          setESymbols(lhs)
          setESymbols(rhs)
        case Times(lhs: ExprTree, rhs: ExprTree) =>
          setESymbols(lhs)
          setESymbols(rhs)
        case Div(lhs: ExprTree, rhs: ExprTree) =>
          setESymbols(lhs)
          setESymbols(rhs)
        case LessThan(lhs: ExprTree, rhs: ExprTree) =>
          setESymbols(lhs)
          setESymbols(rhs)
        case Equals(lhs: ExprTree, rhs: ExprTree) =>
          setESymbols(lhs)
          setESymbols(rhs)
        case ArrayRead(arr: ExprTree, index: ExprTree) =>
          setESymbols(arr)
          setESymbols(index)
        case ArrayLength(arr: ExprTree) =>
          setESymbols(arr)
        case MethodCall(obj: ExprTree, meth: Identifier, args: List[ExprTree]) =>
          setESymbols(obj)
          args.foreach(setESymbols)
        case Variable(id: Identifier) => {
          setISymbol(id)
        }
        case t: This => ms match{
          case Some(m) => t.setSymbol(m.classSymbol)
          case None => error(s"This cannot be called in main Object.", t)
        }
        case NewIntArray(size: ExprTree) => setESymbols(size)
        case New(tpe: Identifier) => {
          gs.lookupClass(tpe.value) match{
            case Some(t) => tpe.setSymbol(t)
            case None => error(s"Type ${tpe.value} is not defined.", tpe)
          }
        }
        case Not(expr: ExprTree) =>setESymbols(expr)
        case _ =>
      }
    }

    def setTypeSymbol(tpe: TypeTree, gs: GlobalScope): Unit = {
      tpe match{
        case ClassType(id) =>
          gs.lookupClass(id.value) match{
            case Some(symbol) =>
              id.setSymbol(symbol)
            case None => error(s"Type ${id.value} is not defined.", id)
          }
        case _ =>
      }
    }

    val gs = collectSymbols(prog)

    terminateIfErrors()

    setPSymbols(prog, gs)

    prog
  }
}
