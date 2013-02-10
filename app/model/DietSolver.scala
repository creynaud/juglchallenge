package model

import collection.mutable

class DietSolver(activities: List[Activity]) {

  def solution(): Set[Activity] = {
    val solution: mutable.Set[Activity] = mutable.Set[Activity]()
    for (activity <- activities) {
      activity.name match {
        case "coca-light" => solution += activity
        case "croissant" => solution += activity
        case "guitar-hero" => solution += activity
        case _ =>
      }
    }
    return solution.toSet
  }

  override def toString(): String = {
    val sb = new StringBuilder()
    sb.append("Diet solver with activities:\n")
    for (activity <- activities) {
      sb.append(activity)
      sb.append("\n")
    }
    return sb.mkString
  }
}
