package model

import collection.mutable
import collection.mutable.ListBuffer

class DietSolver(activities: List[Activity]) {

  def solution(): Set[Activity] = {
    var sumAllNegatives: Int = 0
    var sumAllPositives: Int = 0
    for (activity <- activities) {
      if (activity.value > 0) {
        sumAllPositives += activity.value
      } else {
        sumAllNegatives += activity.value
      }
    }
    val solution: mutable.Map[(Int, Int), ListBuffer[Int]] = mutable.Map[(Int, Int), ListBuffer[Int]]()
    solve(sumAllNegatives, sumAllPositives, activities.length - 1, 0, solution)
    val solutionIndexesList: Option[ListBuffer[Int]] = solution.get(activities.length - 1, 0)
    val result: mutable.Set[Activity] = mutable.Set[Activity]()
    var checksum: Int = 0
    for (index <- solutionIndexesList.get) {
      result += activities(index)
      checksum += activities(index).value
    }
    assert (checksum == 0)
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
  def solve(sumAllNegatives: Int, sumAllPositives: Int, index: Int, sum: Int, solution: mutable.Map[(Int, Int), ListBuffer[Int]]) {
    if (sum < sumAllNegatives) {
      solution.put((index, sum), ListBuffer[Int]())
      return
    } else if (sum > sumAllPositives) {
      solution.put((index, sum), ListBuffer[Int]())
      return
    }
    if (index == 0) {
      if (sum == activities(0).value) {
        solution.put((0, sum), ListBuffer[Int](0))
      } else {
        solution.put((0, sum), ListBuffer[Int]())
      }
    } else {
      val existingSolutionIndexMinus1Sum: Option[ListBuffer[Int]] = solution.get((index - 1, sum))
      existingSolutionIndexMinus1Sum match {
        case None => solve(sumAllNegatives, sumAllPositives, index - 1, sum, solution)
        case _ =>
      }
      val existingSolutionIndexMinus1SumMinusValue: Option[ListBuffer[Int]] = solution.get((index - 1, sum - activities(index).value))
      existingSolutionIndexMinus1SumMinusValue match {
        case None => solve(sumAllNegatives, sumAllPositives, index - 1, sum - activities(index).value, solution)
        case _ =>
      }

      val solutionIndexMinus1Sum: ListBuffer[Int] = solution.get((index - 1, sum)).get
      if (solutionIndexMinus1Sum.length > 0) {
        solution.put((index, sum), solutionIndexMinus1Sum)
      }
      if (activities(index).value == sum) {
        solution.put((index, sum), ListBuffer[Int](index))
      }
      val solutionIndexMinus1SumMinusValue: ListBuffer[Int] = solution.get((index - 1, sum - activities(index).value)).get
      if (solutionIndexMinus1SumMinusValue.length > 0) {
        solutionIndexMinus1SumMinusValue += index
        solution.put((index, sum), solutionIndexMinus1SumMinusValue)
      }

      val existingSolutionIndexSum: Option[ListBuffer[Int]] = solution.get((index, sum))
      existingSolutionIndexSum match {
        case None => solution.put((index, sum), ListBuffer[Int]())
        case _ =>
      }
    }
  }
}
