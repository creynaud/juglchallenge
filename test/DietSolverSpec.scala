package test

import org.specs2.mutable._
import model.{Activity, DietSolver}
import collection.mutable.ListBuffer

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
      val activitiesListBuffer = ListBuffer[Activity]()
      activitiesListBuffer += cocaLight
      activitiesListBuffer += croissant
      activitiesListBuffer += travailAVelo
      activitiesListBuffer += guitarHero
      val solver: DietSolver = new DietSolver(activitiesListBuffer.toList)
      solver.solution() must equalTo(Set(cocaLight, croissant, guitarHero))
    }
  }
}
