import scala.quoted.*

object Macro:

  inline def f(): Unit = ${ macroImplementation }

  def macroImplementation(using Quotes): Expr[Unit] =
    import quotes.reflect.*
    report.error("some error", Position.ofMacroExpansion)
    '{ println("Implementation in MacroCompileError") }
