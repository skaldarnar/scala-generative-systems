package data

class Terminal(override val name: String) extends Symbol(name) {
  override def getChildren: Seq[Symbol] = Seq()
}
