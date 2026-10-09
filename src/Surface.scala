import Common.*

object Surface:
  final case class Defs(defs: List[Def]):
    override def toString: String = defs.mkString("\n")

  final case class Def(name: Name, value: Tm):
    override def toString: String = s"def $name = $value"

  enum Tm:
    case NumberLit(value: String)
    case Var(name: Name)
    case App(fn: Tm, arg: Tm)
    case Lam(param: Name, body: Tm)
    case Let(param: Name, value: Tm, body: Tm)

    override def toString: String =
      this match
        case NumberLit(v) => s"$v"
        case Var(x)       => s"$x"
        case App(fn, arg) => s"($fn $arg)"
        case Lam(x, b)    => s"(\\$x => $b)"
        case Let(x, v, b) => s"(let $x = $v; $b)"
