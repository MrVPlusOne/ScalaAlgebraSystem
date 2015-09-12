package core

trait UserArg{
  def posName: Symb
  def meaning: Expr
}

case class Binding(posName: Symb, meaning: Expr) extends UserArg

class EvalEnv {
  private var userFuncs = List[UserFunc]()

  def addUserFunc(name: String, vars: UserArg*): UserFunc = {
    val args = vars.toIndexedSeq
    val f = UserFunc(name, args.map(_.posName), args.map(_.meaning), IndexedSeq())
    userFuncs = f :: userFuncs
    f
  }

  def envString: String = {
    userFuncs.map(mapString).mkString("\n")
  }

  def mapString(f: UserFunc):String ={
    val name = f.name
    val argNames = f.posNames.map(_.nicePrint).mkString(",")
    s"$name -> $name($argNames)"
  }
}

object EvalEnv{
  val empty = new EvalEnv
}
