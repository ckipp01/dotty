import scala.util.continuations.*

class Baz:
  def foo = shiftUnit[Foo with BarA, Unit, Unit](null)
