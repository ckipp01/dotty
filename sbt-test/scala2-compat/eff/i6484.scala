import cats.*
import cats.data.*
import cats.implicits.*
import org.atnos.eff.*
import org.atnos.eff.all.*
import org.atnos.eff.syntax.all.*

import scala.language.implicitConversions

object Test:
  def resolve[T](e: Eff[Fx1[[X] =>> Kleisli[Id, String, X]], T], g: String): T =
    e.runReader[String](g)(Member.Member1[[X] =>> Kleisli[Id, String, X]]).run
