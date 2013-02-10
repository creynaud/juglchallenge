package test

import org.specs2.mutable._

import controllers._

/**
 * Tests for the mine sweeper game.
 */
class MineSweeperSolverSpec extends Specification {

  "MineSweeperSolver" should {
    "parse a 4x4 mine sweeper string" in {
      val solver: MineSweeperSolver = MineSweeperSolver.parse("4 4\n*...\n....\n.*..\n....")
      solver.toString() must equalTo("4 4\n*...\n....\n.*..\n....")
    }
    "solve a 4x4 mine sweeper" in {
      val solver: MineSweeperSolver = MineSweeperSolver.parse("4 4\n*...\n....\n.*..\n....")
      solver.solution() must equalTo("*100\n2210\n1*10\n1110")
    }
    "parse a 3x5 mine sweeper string" in {
      val solver: MineSweeperSolver = MineSweeperSolver.parse("3 5\n**...\n.....\n.*...")
      solver.toString() must equalTo("3 5\n**...\n.....\n.*...")
    }
    "solve a 3x5 mine sweeper" in {
      val solver: MineSweeperSolver = MineSweeperSolver.parse("3 5\n**...\n.....\n.*...")
      solver.solution() must equalTo("**100\n33200\n1*100")
    }
  }
}
