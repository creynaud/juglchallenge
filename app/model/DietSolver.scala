package model

import collection.mutable

class DietSolver(activities: List[Activity]) {

  def solution(): Set[Activity] = {
    var addNegativesSum: Int = 0
    var allPositivesSum: Int = 0
    for (activity <- activities) {
      if (activity.value > 0) {
        allPositivesSum += activity.value
      } else {
        addNegativesSum += activity.value
      }
    }
    val solutionsMap: mutable.Map[(Int, Int), Set[Int]] = mutable.Map[(Int, Int), Set[Int]]()
    solveSubsetSum(addNegativesSum, allPositivesSum, activities.length - 1, 0, solutionsMap)
    val solutionIndexes: Option[Set[Int]] = solutionsMap.get(activities.length - 1, 0)
    val result: mutable.Set[Activity] = mutable.Set[Activity]()
    var checksum: Int = 0
    for (index <- solutionIndexes.get) {
      result += activities(index)
      checksum += activities(index).value
    }
    assert(checksum == 0)
    return result.toSet
  }

  // http://en.wikipedia.org/wiki/Subset_sum_problem#Pseudo-polynomial_time_dynamic_programming_solution
  def solveSubsetSum(allNegativesSum: Int, allPositivesSum: Int, indexDefiningCurrentSet: Int, sum: Int, solutionsMap: mutable.Map[(Int, Int), Set[Int]]) {
    // If sum is smaller than all the negatives, or greater than all the positives, there is no solution
    if (sum < allNegativesSum) {
      solutionsMap.put((indexDefiningCurrentSet, sum), Set[Int]())
      return
    } else if (sum > allPositivesSum) {
      solutionsMap.put((indexDefiningCurrentSet, sum), Set[Int]())
      return
    }

    // If current set contains only the 1st element, there is a solution if its value is sum
    if (indexDefiningCurrentSet == 0) {
      if (sum == activities(0).value) {
        solutionsMap.put((0, sum), Set[Int](0))
      } else {
        solutionsMap.put((0, sum), Set[Int]())
      }
      return
    }

    // If current element's value is sum, the singleton set containing this element is a solution
    if (activities(indexDefiningCurrentSet).value == sum) {
      solutionsMap.put((indexDefiningCurrentSet, sum), Set[Int](indexDefiningCurrentSet))
      return
    }

    // Let's find out if there is a solution for this set minus the last element and sum
    val existingSolutionIndexMinus1Sum: Option[Set[Int]] = solutionsMap.get((indexDefiningCurrentSet - 1, sum))
    existingSolutionIndexMinus1Sum match {
      case None => solveSubsetSum(allNegativesSum, allPositivesSum, indexDefiningCurrentSet - 1, sum, solutionsMap)
      case _ =>
    }
    // If there is a solution for this set minus the last element, it is also a solution for this set
    val solutionIndexMinus1Sum: Set[Int] = solutionsMap.get((indexDefiningCurrentSet - 1, sum)).get
    if (solutionIndexMinus1Sum.size > 0) {
      solutionsMap.put((indexDefiningCurrentSet, sum), solutionIndexMinus1Sum.toSet)
      return
    }

    // Let's find out if there is a solution for this set minus the last element and sum minus last element value
    val existingSolutionIndexMinus1SumMinusValue: Option[Set[Int]] = solutionsMap.get((indexDefiningCurrentSet - 1, sum - activities(indexDefiningCurrentSet).value))
    existingSolutionIndexMinus1SumMinusValue match {
      case None => solveSubsetSum(allNegativesSum, allPositivesSum, indexDefiningCurrentSet - 1, sum - activities(indexDefiningCurrentSet).value, solutionsMap)
      case _ =>
    }
    // If there is a solution for this set minus the last element, it is also a solution for this set
    val solutionIndexMinus1SumMinusValue: Set[Int] = solutionsMap.get((indexDefiningCurrentSet - 1, sum - activities(indexDefiningCurrentSet).value)).get
    if (solutionIndexMinus1SumMinusValue.size > 0) {
      val solutionIndexSumMinusValue: mutable.Set[Int] = mutable.Set[Int]()
      solutionIndexSumMinusValue += indexDefiningCurrentSet
      solutionIndexSumMinusValue ++= solutionIndexMinus1SumMinusValue
      solutionsMap.put((indexDefiningCurrentSet, sum), solutionIndexSumMinusValue.toSet)
      return
    }

    // There is no solution
    solutionsMap.put((indexDefiningCurrentSet, sum), Set[Int]())
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
