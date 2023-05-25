import scala.util.continuations.*

class Use:
  val a = new Baz
  def bar: (Foo with BarA) @cpsParam[Unit, Unit] = a.foo
