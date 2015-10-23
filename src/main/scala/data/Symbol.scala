package data

abstract class Symbol(val name: String) {

  def getChildren: Seq[Symbol] = ???

  override def toString = name

}
