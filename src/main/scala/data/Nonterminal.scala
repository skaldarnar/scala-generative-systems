package data

class Nonterminal(override val name: String) extends Symbol(name) {

  var children: Seq[Symbol] = Seq()

  def setChildren(children: Seq[Symbol]): Unit = {
    this.children = children
  }

  override def getChildren: Seq[Symbol] = children

  def ->(lhs: Seq[Symbol]): Production = {
    new Production(this, lhs)
  }
}
