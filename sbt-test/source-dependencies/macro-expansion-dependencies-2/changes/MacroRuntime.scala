import scala.quoted.*

object MacroRuntime:

  def impl()(using Quotes): Expr[Unit] =
    '{ println("Implementation in Macro") }
