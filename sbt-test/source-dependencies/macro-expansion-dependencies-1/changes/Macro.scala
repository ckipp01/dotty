import scala.quoted.*

object Macro:

  inline def f(): Unit = ${ macroImplementation }

  def macroImplementation(using Quotes): Expr[Unit] =
    '{ println("Implementation in Macro") }
