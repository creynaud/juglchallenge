package test

import org.specs2.mutable._
import model.{Activity, DietSolver}
import collection.mutable

/**
 * Tests for the mine sweeper game.
 */
class DietSolverSpec extends Specification {

  "DietSolver" should {
    "Find a solution" in {
      val cocaLight = new Activity("coca-light", 1)
      val croissant = new Activity("croissant", 180)
      val travailAVelo = new Activity("au-travail-a-velo", -113)
      val guitarHero = new Activity("guitar-hero", -181)
      val activitiesListBuffer = mutable.ListBuffer[Activity]()
      activitiesListBuffer += cocaLight
      activitiesListBuffer += croissant
      activitiesListBuffer += travailAVelo
      activitiesListBuffer += guitarHero
      val solver: DietSolver = new DietSolver(activitiesListBuffer.toList)
      solver.solution() must equalTo(Set(cocaLight, croissant, guitarHero))
    }
    "Find a solution" in {
      val activitiesListBuffer = mutable.ListBuffer[Activity]()
      activitiesListBuffer += new Activity("coca-light", 1)
      activitiesListBuffer += new Activity("ga", 1)
      activitiesListBuffer += new Activity("bu", 42)
      activitiesListBuffer += new Activity("zo", 3)
      activitiesListBuffer += new Activity("meuh", -112)
      activitiesListBuffer += new Activity("croissant", 180)
      activitiesListBuffer += new Activity("kinder", 212)
      activitiesListBuffer += new Activity("burger", 612)
      activitiesListBuffer += new Activity("au-travail-a-velo", -113)
      activitiesListBuffer += new Activity("standing-desk", -400)
      activitiesListBuffer += new Activity("guitar-hero", -181)
      val solver: DietSolver = new DietSolver(activitiesListBuffer.toList)
      val canBeSolved: mutable.Map[(Int, Int), Boolean] = mutable.Map[(Int, Int), Boolean]()
      val solution: mutable.Map[(Int, Int), mutable.ListBuffer[Int]] = mutable.Map[(Int, Int), mutable.ListBuffer[Int]]()
      solver.solve(activitiesListBuffer.length -1, 0, canBeSolved, solution)
      canBeSolved.get((activitiesListBuffer.length -1, 0)).get mustEqual(true)
    }
    "Find no solution" in {
      val activitiesListBuffer = mutable.ListBuffer[Activity]()
      activitiesListBuffer += new Activity("coca-light", 1)
      activitiesListBuffer += new Activity("ga", 1)
      activitiesListBuffer += new Activity("bu", 42)
      activitiesListBuffer += new Activity("zo", 3)
      activitiesListBuffer += new Activity("meuh", 112)
      activitiesListBuffer += new Activity("croissant", 180)
      activitiesListBuffer += new Activity("kinder", 212)
      activitiesListBuffer += new Activity("burger", 612)
      activitiesListBuffer += new Activity("au-travail-a-velo", 113)
      activitiesListBuffer += new Activity("standing-desk", 400)
      activitiesListBuffer += new Activity("guitar-hero", 181)
      val solver: DietSolver = new DietSolver(activitiesListBuffer.toList)
      val canBeSolved: mutable.Map[(Int, Int), Boolean] = mutable.Map[(Int, Int), Boolean]()
      val solution: mutable.Map[(Int, Int), mutable.ListBuffer[Int]] = mutable.Map[(Int, Int), mutable.ListBuffer[Int]]()
      solver.solve(activitiesListBuffer.length -1, 0, canBeSolved, solution)
      canBeSolved.get((activitiesListBuffer.length -1, 0)).get mustEqual(false)
      solver.solution() must equalTo(Set())
    }
  }
}
