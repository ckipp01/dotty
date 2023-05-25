object C:
  import A.*, B.*
  implicitly[Ordering[Int]]

  def main(args: Array[String]): Unit = ()
