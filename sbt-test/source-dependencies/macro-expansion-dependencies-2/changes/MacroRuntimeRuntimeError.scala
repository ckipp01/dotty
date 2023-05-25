import scala.quoted.*

object MacroRuntime:

  def impl()(using q: Quotes): Expr[Unit] =
    '{ ??? }
