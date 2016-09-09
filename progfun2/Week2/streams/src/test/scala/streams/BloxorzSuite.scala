package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) => move match {
        case Left => block.left
        case Right => block.right
        case Up => block.up
        case Down => block.down
      }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }


	test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(terrain(Pos(1,1)), "1,1") // start
      assert(terrain(Pos(4,7)), "4,7") // goal
      assert(terrain(Pos(5,8)), "5,8")
      assert(!terrain(Pos(5,9)), "5,9")
      assert(terrain(Pos(4,9)), "4,9")
      assert(!terrain(Pos(6,8)), "6,8")
      assert(!terrain(Pos(4,11)), "4,11")
      assert(!terrain(Pos(-1,0)), "-1,0")
      assert(!terrain(Pos(0,-1)), "0,-1")
      // exceed terrain size
      assert(!terrain(Pos(3,10)), "3,10")
      assert(!terrain(Pos(6,0)), "6,0")
      assert(!terrain(Pos(6,10)), "6,10")
    }
  }

	test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
      assert(goal == Pos(4,7))
    }
  }

  test("move tests for level 1") {
    new Level1 {
      assert(startBlock.isLegal, "StartBlock is legal")
      assert(startBlock.isStanding, "StartBlock is standing")
      assert(startBlock.neighbors == List((Block(Pos(1,-1),Pos(1,0)),Left), (Block(Pos(-1,1),Pos(0,1)),Up), (Block(Pos(1,2),Pos(1,3)),Right), (Block(Pos(2,1),Pos(3,1)),Down)), "all neighbors of startPos")
      assert(startBlock.legalNeighbors == List((Block(Pos(1,2),Pos(1,3)),Right), (Block(Pos(2,1),Pos(3,1)),Down)), "all legalNeighbors of startPos")
    }
  }

  test("myTest_1") {
    new Level1 {
      val assertionSet = Set(
        (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
        (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
      )
      assert(neighborsWithHistory(Block(Pos(1,1),Pos(1,1)), List(Left,Up)).toSet == assertionSet, "allowed moves")
    }
  }

  test("myTest_2") {
    new Level1 {
      val assertionSet = Set((Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up)))
      val n = Set((Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
                  (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))).toStream
      val e = Set(Block(Pos(1,2),Pos(1,3)), Block(Pos(1,1),Pos(1,1)))
      assert(newNeighborsOnly(n, e).toSet == assertionSet, "newNeighborsOnly")
    }
  }

	test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }


	test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }

}
