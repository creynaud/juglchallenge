package test

import org.specs2.mutable._

import play.api.test._
import play.api.test.Helpers._
import controllers._

/**
 * Add your spec here.
 * You can mock out a whole application including requests, plugins etc.
 * For more information, consult the wiki.
 */
class MineSweeperSolverSpec extends Specification {

  "MineSweeperSolver" should {
    "parse a 4x4 string" in {
      val solver: MineSweeperSolver = MineSweeperSolver.parse("4 4\n*...\n....\n.*..\n....")
      solver.toString() must equalTo("4 4\n*...\n....\n.*..\n....")
    }
    "solve it" in {
      val solver: MineSweeperSolver = MineSweeperSolver.parse("4 4\n*...\n....\n.*..\n....")
      solver.solution() must equalTo("*100\n2210\n1*10\n1110")
    }
    "parse a 3x5 string" in {
      val solver: MineSweeperSolver = MineSweeperSolver.parse("3 5\n**...\n.....\n.*...")
      solver.toString() must equalTo("3 5\n**...\n.....\n.*...")
    }
    "solve it" in {
      val solver: MineSweeperSolver = MineSweeperSolver.parse("3 5\n**...\n.....\n.*...")
      solver.solution() must equalTo("**100\n33200\n1*100")
    }
  }
}
