import scala.util.continuations.*

class Baz:
  def foo = shiftUnit[Foo with BarB, Unit, Unit](null)
