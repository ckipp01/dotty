import scala.quoted._

object Macro {

  trait AddInt[A <: Int, B <: Int] { type Out <: Int }

  transparent inline def ff[A <: Int, B <: Int](): AddInt[A, B] = ${ impl[A, B] }

  def impl[A <: Int : Type, B <: Int : Type](using qctx: QuoteContext) : Expr[AddInt[A, B]] = {
    import qctx.reflect._

    val ConstantType(Constant.Int(v1)) = Type.of[A]
    val ConstantType(Constant.Int(v2)) = Type.of[B]

    Literal(Constant.Int(v1 + v2)).tpe.seal match
      case '[$T] => '{ null: AddInt[A, B] { type Out = T } }
  }
}
