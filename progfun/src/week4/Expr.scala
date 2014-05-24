package week4

trait Expr {
  def eval: Int = this match {
    case Number(x) => x
    case Sum(left: Expr, right: Expr) => left.eval + right.eval
    case Var(value) => 0
    case Prod(left: Expr, right: Expr) => left.eval * right.eval
  }
}

case class Number(n: Int) extends Expr

case class Sum(left: Expr, right: Expr) extends Expr

case class Var(value: String) extends Expr

case class Prod(left: Expr, right: Expr) extends Expr