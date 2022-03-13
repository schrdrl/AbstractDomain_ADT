package AList_CleanCode

case class AState(vars: Map[String, AVal]) {
  def lookup(name: String) = vars(name)
  def updated(name: String, value: AVal) = AState(vars + (name -> value))

  def widen(that: AState): AState = {
    val names = this.vars.keySet union that.vars.keySet
    val vars =
      for (name <- names) yield {
        val a = this.lookup(name)
        val b = that.lookup(name)
        name -> (a widen b)
      }
    AState(vars.toMap)
  }
}

object AState {
  def widenAll(as: Set[AState]): Set[AState] = {
    if (as.isEmpty)
      Set()
    else
      Set(as.reduceLeft(_ widen _))
  }
}
