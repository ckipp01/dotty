import scala.quoted.*

object Macros:

  inline def foo: A = ${ fooImpl }

  def fooImpl(using Quotes): Expr[A] =
    new B().f
    '{ new A; }
