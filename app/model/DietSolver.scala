package model

import collection.mutable
import collection.mutable.ListBuffer

class DietSolver(activities: List[Activity]) {

  def solution(): Set[Activity] = {
    val solution: mutable.Map[(Int, Int), ListBuffer[Int]] = mutable.Map[(Int, Int), ListBuffer[Int]]()
    solve(activities.length -1, 0, solution)
    val solutionIndexesList: Option[ListBuffer[Int]] = solution.get(activities.length - 1, 0)
    solutionIndexesList match {
      case Some(_) => {
        val result: mutable.Set[Activity] = mutable.Set[Activity]()
        for (index <- solutionIndexesList.get) {
          result += activities(index)
        }
        return result.toSet
      }
      case _ => return Set()
    }
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

  // http://en.wikipedia.org/wiki/Subset_sum_problem#Pseudo-polynomial_time_dynamic_programming_solution
  def solve(index: Int, sum: Int, solution: mutable.Map[(Int, Int), ListBuffer[Int]]) {
    if (index == 0) {
      if (sum == activities(0).value) {
        solution.put((0, sum), ListBuffer[Int](0))
      }
    } else {
      solve(index - 1, sum, solution)
      solve(index - 1, sum - activities(index).value, solution)
      val solutionOption: Option[ListBuffer[Int]] = solution.get((index - 1, sum))
      solutionOption match {
        case Some(_) => {
          val solutionForIndexMinus1: ListBuffer[Int] = solution.get((index - 1, sum)).get
          solution.put((index, sum), solutionForIndexMinus1)
        }
        case _ =>
      }
      if (activities(index).value == sum) {
        solution.put((index, sum), ListBuffer[Int](index))
      }
      val otherSolutionOption: Option[ListBuffer[Int]] = solution.get((index - 1, sum - activities(index).value))
      otherSolutionOption match {
        case Some(_) => {
          val solutionForIndexMinus1: ListBuffer[Int] = solution.get((index - 1, sum - activities(index).value)).get
          solutionForIndexMinus1 += index
          solution.put((index, sum), solutionForIndexMinus1)
        }
        case _ =>
      }
    }
  }
}
