package test

import org.specs2.mutable._
import model.{Activity, DietSolver}
import collection.mutable

/**
 * Tests for the mine sweeper game.
 */
class DietSolverSpec extends Specification {

  "DietSolver" should {
    "Find a solution in a small set of activies" in {
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
    "Find a solution in a bigger set of activities" in {
      val activitiesListBuffer = mutable.ListBuffer[Activity]()
      activitiesListBuffer += new Activity("coca-light", 1)
      val ga = new Activity("ga", 1)
      activitiesListBuffer += ga
      val bu = new Activity("bu", 42)
      activitiesListBuffer += bu
      val zo = new Activity("zo", 3)
      activitiesListBuffer += zo
      val meuh = new Activity("meuh", -112)
      activitiesListBuffer += meuh
      val croissant = new Activity("croissant", 180)
      activitiesListBuffer += croissant
      activitiesListBuffer += new Activity("kinder", 212)
      activitiesListBuffer += new Activity("burger", 612)
      val travailAVelo = new Activity("au-travail-a-velo", -113)
      activitiesListBuffer += travailAVelo
      activitiesListBuffer += new Activity("standing-desk", -400)
      val guitarHero = new Activity("guitar-hero", -181)
      activitiesListBuffer += guitarHero
      val solver: DietSolver = new DietSolver(activitiesListBuffer.toList)
      solver.solution() must equalTo(Set(travailAVelo, croissant, bu, zo, meuh))
    }
    "Find no solution in a set of activities" in {
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
      solver.solution() must equalTo(Set())
    }
  }
}
