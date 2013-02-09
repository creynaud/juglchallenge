package controllers

import collection.mutable._

object MineSweeperSolver {
  def parse(text: String): MineSweeperSolver = {
    val minesListBuffer = ListBuffer[(Int, Int)]()
    var currentLine: Int = 0
    var size = 0
    for (line <- text.split("[\\n-\\r]+")) {
      if (line.contains(".") || line.contains("*")) {
        var currentChar: Int = 0
        for (c <- line) {
          if (c == '*') {
            minesListBuffer += ((currentLine, currentChar))
          }
          currentChar = currentChar + 1
        }
        currentLine = currentLine + 1
      } else {
        val sizes = line.split("\\s+")
        for (sizeString <- sizes) {
          size = sizeString.toInt
        }
      }
    }
    new MineSweeperSolver(size, minesListBuffer.toList)
  }
}

class MineSweeperSolver(size: Int, mines: List[(Int, Int)]) {

  def solution(): String = {
    val sb = new StringBuilder()
    for (line <- Range(0, size)) {
      for (column <- Range(0, size)) {
        if (mines.contains((line, column))) {
          sb.append("*")
        } else {
          var numberOfSurroundingMines = 0
          if (mines.contains((line, column - 1))) {
            numberOfSurroundingMines += 1
          }
          if (mines.contains((line, column + 1))) {
            numberOfSurroundingMines += 1
          }
          if (mines.contains((line - 1, column))) {
            numberOfSurroundingMines += 1
          }
          if (mines.contains((line + 1, column))) {
            numberOfSurroundingMines += 1
          }
          if (mines.contains((line - 1, column - 1))) {
            numberOfSurroundingMines += 1
          }
          if (mines.contains((line - 1, column + 1))) {
            numberOfSurroundingMines += 1
          }
          if (mines.contains((line + 1, column - 1))) {
            numberOfSurroundingMines += 1
          }
          if (mines.contains((line + 1, column + 1))) {
            numberOfSurroundingMines += 1
          }
          sb.append(numberOfSurroundingMines)
        }
        if ((column == size - 1) && (line != size - 1)) {
          sb.append("\n")
        }
      }
    }
    return sb.mkString
  }

  override def toString(): String = {
    val sb = new StringBuilder()
    sb.append(size)
    sb.append(" ")
    sb.append(size)
    sb.append("\n")
    for (line <- Range(0, size)) {
      for (column <- Range(0, size)) {
        if (mines.contains((line, column))) {
          sb.append("*")
        } else {
          sb.append(".")
        }
        if ((column == size - 1) && (line != size - 1)) {
          sb.append("\n")
        }
      }
    }
    return sb.mkString
  }
}
