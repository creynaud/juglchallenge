package model

import collection.mutable
import collection.mutable.ListBuffer

class DietSolver(activities: List[Activity]) {

  def solution(): Set[Activity] = {
    var n: Int = 0
    var p: Int = 0
    for (activity <- activities) {
      if (activity.value > 0) {
        p += activity.value
      } else {
        n += activity.value
      }
    }
    val solution: mutable.Map[(Int, Int), ListBuffer[Int]] = mutable.Map[(Int, Int), ListBuffer[Int]]()
    solve(n, p, activities.length -1, 0, solution)
    val solutionIndexesList: Option[ListBuffer[Int]] = solution.get(activities.length - 1, 0)
    val result: mutable.Set[Activity] = mutable.Set[Activity]()
    for (index <- solutionIndexesList.get) {
      result += activities(index)
    }
    return result.toSet
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
  def solve(n: Int, p: Int, index: Int, sum: Int, solution: mutable.Map[(Int, Int), ListBuffer[Int]]) {
    solution.put((index, sum), ListBuffer[Int]())
    if (sum < n) {
      return
    } else if (sum > p) {
      return
    }
    if (index == 0) {
      if (sum == activities(0).value) {
        solution.put((index, sum), ListBuffer[Int](0))
      }
    } else {
      val existingSolutionOption: Option[ListBuffer[Int]] = solution.get((index - 1, sum))
      existingSolutionOption match {
        case None => solve(n, p, index - 1, sum, solution)
        case _ =>
      }
      val otherExistingSolutionOption: Option[ListBuffer[Int]] = solution.get((index - 1, sum - activities(index).value))
      otherExistingSolutionOption match {
        case None => solve(n, p, index - 1, sum - activities(index).value, solution)
        case _ =>
      }

      val solutionForIndexMinus1: ListBuffer[Int] = solution.get((index - 1, sum)).get
      if (solutionForIndexMinus1.length > 0) {
        solution.put((index, sum), solutionForIndexMinus1)
      }
      if (activities(index).value == sum) {
        solution.put((index, sum), ListBuffer[Int](index))
      }
      val solutionForIndexMinus1SumMinusValue: ListBuffer[Int] = solution.get((index - 1, sum - activities(index).value)).get
      if (solutionForIndexMinus1SumMinusValue.length > 0) {
        solutionForIndexMinus1SumMinusValue += index
        solution.put((index, sum), solutionForIndexMinus1SumMinusValue)
      }
    }
  }
}
