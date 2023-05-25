TaskKey[Unit]("output-empty") := {
  val outputDirectory = (classDirectory in Compile).value
  val classes = (outputDirectory ** "*.class").get
  if classes.nonEmpty then
    sys.error("Classes existed:\n\t" + classes.mkString("\n\t"))
  else ()
}
