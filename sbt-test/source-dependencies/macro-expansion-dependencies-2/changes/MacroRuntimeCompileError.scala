import scala.quoted.*

object MacroRuntime:

  def impl()(using q: Quotes): Expr[Unit] =
    import quotes.reflect.*
    report.error("some error", Position.ofMacroExpansion)
    '{ println("Implementation in MacroCompileError") }
