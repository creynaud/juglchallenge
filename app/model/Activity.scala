package model

class Activity(aName: String, aValue: Int) {
  val name: String = aName
  val value: Int = aValue

  override def toString(): String = ("name: " + name + ", value: " + value)
}
