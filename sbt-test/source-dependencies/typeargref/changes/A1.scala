import scala.jdk.CollectionConverters.*
import java.util.{Map as JMap}

class A:
  // Inferred type for `param`: java.util.Map[Int, _ <: Int]#<parameter V of trait Map>
  def param =
    val opt: Option[JMap[Int, ? <: Int]] = None
    opt.getOrElse(Map.empty.asJava).get(42)
